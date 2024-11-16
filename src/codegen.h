#pragma once

#include "lexer.h"
#include "parser.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Utils/Mem2Reg.h"

#include <algorithm>
#include <alloca.h>
#include <cstdio>
#include <sys/types.h>

namespace Codegen {

static llvm::LLVMContext ctx;
static llvm::IRBuilder<> builder(ctx);
static llvm::Module module("hex compiler", ctx);
static llvm::StringMap<llvm::AllocaInst *> named_values;

static llvm::FunctionPassManager FPM;
static llvm::LoopAnalysisManager LAM;
static llvm::FunctionAnalysisManager FAM;
static llvm::CGSCCAnalysisManager CGAM;
static llvm::ModuleAnalysisManager MAM;
static llvm::PassInstrumentationCallbacks PIC;
static llvm::StandardInstrumentations SI(ctx, true);

static void initialize_optimizations() {
  SI.registerCallbacks(PIC, &MAM);
  FPM.addPass(llvm::PromotePass());
  // simple peephole optimizations, bit twiddling
  FPM.addPass(llvm::InstCombinePass());
  // reassociate expressions
  FPM.addPass(llvm::ReassociatePass());
  // eliminate common subexpressions
  // FPM.addPass(llvm::GVNPass());
  // simply cfg
  FPM.addPass(llvm::SimplifyCFGPass());

  llvm::PassBuilder PB;
  PB.registerModuleAnalyses(MAM);
  PB.registerFunctionAnalyses(FAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
}

static llvm::Value *codegen_error(const char *msg) {
  fprintf(stderr, "error generating code: %s", msg);
  return nullptr;
}

static llvm::AllocaInst *create_entry_block_alloca(llvm::Function *func,
                                                   llvm::StringRef var_name) {
  llvm::IRBuilder<> tmp_builder(&func->getEntryBlock(),
                                func->getEntryBlock().begin());
  return tmp_builder.CreateAlloca(llvm::Type::getDoubleTy(ctx), nullptr,
                                  var_name);
}

static llvm::Value *generate(Arena *arena, AstExpr *expr) {
  auto tmp = arena->begin_temp();
  switch (expr->type) {
  case AST_NUMERIC_EXPR: {
    return llvm::ConstantFP::get(ctx, llvm::APFloat(expr->numeric_expr.val));
  } break;
  case AST_IDENTIFIER_EXPR: {
    const char *name = reinterpret_cast<const char *>(
        materialize_slice(arena, &expr->ident_expr.name));
    llvm::AllocaInst *A = named_values[name];
    if (!A) {
      codegen_error("unknown variable name: ");
      printf("NAME: %s\n", name);
    }
    return builder.CreateLoad(A->getAllocatedType(), A, name);
  } break;
  case AST_INFIX_EXPR: {
    // handle assignment
    if (expr->infix_expr.op == TOKEN_EQUAL) {
      const char *var_name = reinterpret_cast<const char *>(
          materialize_slice(arena, &expr->infix_expr.lhs->ident_expr.name));
      llvm::Value *val = generate(arena, expr->infix_expr.rhs);
      if (!val)
        return nullptr;
      llvm::Value *variable = named_values[var_name];
      if (!variable)
        return codegen_error("unknown variable name in  assignment");
      builder.CreateStore(variable, val);
      return val;
    }

    llvm::Value *lhs = generate(arena, expr->infix_expr.lhs);
    llvm::Value *rhs = generate(arena, expr->infix_expr.rhs);

    if (!lhs || !rhs)
      return nullptr;

    switch (expr->infix_expr.op) {
    case TOKEN_PLUS:
      return builder.CreateFAdd(lhs, rhs, "addtmp");
    case TOKEN_MINUS:
      return builder.CreateFSub(lhs, rhs, "subtmp");
    case TOKEN_STAR:
      return builder.CreateFMul(lhs, rhs, "multmp");
    case TOKEN_SLASH:
      return builder.CreateFDiv(lhs, rhs, "divtmp");
    case TOKEN_LESS:
      lhs = builder.CreateFCmpULT(lhs, rhs, "cmptmp");
      return builder.CreateUIToFP(lhs, llvm::Type::getDoubleTy(ctx), "booltmp");
    default:
      return codegen_error("invalid binary opeartor");
    }

  } break;
  case AST_CALL_EXPR: {
    const char *name = reinterpret_cast<const char *>(
        materialize_slice(arena, &expr->call_expr.name));
    llvm::Function *callee = module.getFunction(name);

    if (!callee)
      return codegen_error("unknown function referenced");

    if (callee->arg_size() != expr->call_expr.args.len())
      return codegen_error("incorrect number of arguments passed");

    llvm::SmallVector<llvm::Value *> argsv;
    for (auto iter = expr->call_expr.args.make_iter(); !iter.done();
         iter.next()) {
      argsv.push_back(generate(arena, *iter.value()));
      if (!argsv.back())
        return nullptr;
    }
    return builder.CreateCall(callee, argsv, "calltmp");
  } break;
  case AST_PROTOTYPE_EXPR: {
    auto proto = expr->prototype_expr;
    const char *name =
        reinterpret_cast<const char *>(materialize_slice(arena, &proto.name));

    llvm::SmallVector<llvm::Type *> doubles(proto.args.len(),
                                            llvm::Type::getDoubleTy(ctx));
    llvm::FunctionType *FT =
        llvm::FunctionType::get(llvm::Type::getDoubleTy(ctx), doubles, false);

    llvm::Function *F = llvm::Function::Create(
        FT, llvm::Function::ExternalLinkage, name, module);

    u32 idx = 0;
    for (auto &arg : F->args()) {
      const char *arg_name = reinterpret_cast<const char *>(
          materialize_slice(arena, &proto.args.get(idx++)->ident_expr.name));
      arg.setName(arg_name);
    }
    return F;
  } break;
  case AST_FUNCTION_EXPR: {
    auto func = expr->function_expr;
    const char *name = reinterpret_cast<const char *>(
        materialize_slice(arena, &func.prototype->prototype_expr.name));

    llvm::Function *F = module.getFunction(name);
    if (!F)
      F = reinterpret_cast<llvm::Function *>(generate(arena, func.prototype));
    if (!F)
      return nullptr;
    if (!F->empty())
      return reinterpret_cast<llvm::Function(*)>(
          codegen_error("function cannot be redefined."));

    llvm::BasicBlock *BB = llvm::BasicBlock::Create(ctx, "entry", F);
    builder.SetInsertPoint(BB);

    named_values.clear();
    for (auto &arg : F->args()) {
      llvm::AllocaInst *alloca = create_entry_block_alloca(F, arg.getName());
      builder.CreateStore(&arg, alloca);
      named_values[arg.getName()] = alloca;
    }

    if (llvm::Value *ret_val = generate(arena, func.body)) {
      builder.CreateRet(ret_val);

      llvm::verifyFunction(*F);

      FPM.run(*F, FAM);
      return F;
    }

    F->eraseFromParent();
    return nullptr;
  } break;
  case AST_IF_EXPR: {
    auto iff = expr->if_expr;

    llvm::Value *condv = generate(arena, iff.cond);
    if (!condv)
      return nullptr;

    condv = builder.CreateFCmpONE(
        condv, llvm::ConstantFP::get(ctx, llvm::APFloat(0.0)), "ifcond");

    llvm::Function *func = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *thenbb = llvm::BasicBlock::Create(ctx, "then", func);
    llvm::BasicBlock *elsebb = llvm::BasicBlock::Create(ctx, "else");
    llvm::BasicBlock *mergebb = llvm::BasicBlock::Create(ctx, "ifcont");

    builder.CreateCondBr(condv, thenbb, elsebb);

    builder.SetInsertPoint(thenbb);
    llvm::Value *thenv = generate(arena, iff.then);
    if (!thenv)
      return nullptr;
    builder.CreateBr(mergebb);
    thenbb = builder.GetInsertBlock();

    func->insert(func->end(), elsebb);
    builder.SetInsertPoint(elsebb);
    llvm::Value *elsev = generate(arena, iff.otherwise);
    if (!elsev)
      return nullptr;
    builder.CreateBr(mergebb);
    elsebb = builder.GetInsertBlock();

    func->insert(func->end(), mergebb);
    builder.SetInsertPoint(mergebb);
    llvm::PHINode *PN =
        builder.CreatePHI(llvm::Type::getDoubleTy(ctx), 2, "iftmp");
    PN->addIncoming(thenv, thenbb);
    PN->addIncoming(elsev, elsebb);
    return PN;
  } break;
  case AST_FOR_EXPR: {
    auto fexpr = expr->for_expr;
    const char *var_name = reinterpret_cast<const char *>(
        materialize_slice(arena, &fexpr.var_name));

    llvm::Function *func = builder.GetInsertBlock()->getParent();

    llvm::AllocaInst *alloca = create_entry_block_alloca(func, var_name);

    llvm::Value *startv = generate(arena, fexpr.start);
    if (!startv)
      return nullptr;

    builder.CreateStore(startv, alloca);

    llvm::BasicBlock *loopbb = llvm::BasicBlock::Create(ctx, "loop", func);
    builder.CreateBr(loopbb);
    builder.SetInsertPoint(loopbb);

    llvm::AllocaInst *oldv = named_values[var_name];
    named_values[var_name] = alloca;

    if (!generate(arena, fexpr.body))
      return nullptr;

    llvm::Value *stepv = (fexpr.step)
                             ? generate(arena, fexpr.step)
                             : llvm::ConstantFP::get(ctx, llvm::APFloat(1.0));
    if (!stepv)
      return nullptr;
    llvm::Value *curr_var =
        builder.CreateLoad(alloca->getAllocatedType(), alloca, var_name);
    llvm::Value *nextv = builder.CreateFAdd(curr_var, stepv, "nextvar");

    llvm::Value *endcond = generate(arena, fexpr.end);
    if (!endcond)
      return nullptr;
    endcond = builder.CreateFCmpONE(
        endcond, llvm::ConstantFP::get(ctx, llvm::APFloat(0.0)), "loopcond");

    llvm::BasicBlock *afterbb =
        llvm::BasicBlock::Create(ctx, "afterloop", func);
    builder.CreateCondBr(endcond, loopbb, afterbb);
    builder.SetInsertPoint(afterbb);

    if (oldv)
      named_values[var_name] = oldv;
    else
      named_values.erase(var_name);

    return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(ctx));
  } break;
  case AST_VAR_EXPR: {
    auto vexpr = expr->var_expr;
    llvm::SmallVector<llvm::AllocaInst *> old_bindings;
    llvm::Function *func = builder.GetInsertBlock()->getParent();
    for (auto iter = vexpr.vars.make_iter(); !iter.done(); iter.next()) {
      auto local = iter.value();
      const char *var_name = reinterpret_cast<const char *>(
          materialize_slice(arena, &local->name));

      llvm::Value *init_val =
          (local->init_val) ? generate(arena, local->init_val)
                            : llvm::ConstantFP::get(ctx, llvm::APFloat(0.0));
      if (!init_val)
        return nullptr;

      llvm::AllocaInst *alloca = create_entry_block_alloca(func, var_name);
      builder.CreateStore(init_val, alloca);
      old_bindings.push_back(named_values[var_name]);
      named_values[var_name] = alloca;
    }

    llvm::Value *body = generate(arena, vexpr.body);
    if (!body)
      return nullptr;
    for (i32 i = 0; i < old_bindings.size(); i++) {
      auto local = vexpr.vars.get(i);
      const char *var_name =
          reinterpret_cast<const char *>(materialize_slice(arena, &local.name));
      named_values[var_name] = old_bindings[i];
    }
    return body;
  } break;
  }
  tmp.reset();
}

static void compile() {
  auto target_triple = llvm::sys::getDefaultTargetTriple();
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  std::string err;
  auto target = llvm::TargetRegistry::lookupTarget(target_triple, err);
  if (!target) {
    llvm::errs() << err;
    return;
  }

  auto CPU = "generic";
  auto features = "";
  llvm::TargetOptions opt;
  auto target_machine = target->createTargetMachine(
      target_triple, CPU, features, opt, llvm::Reloc::PIC_);

  module.setDataLayout(target_machine->createDataLayout());
  module.setTargetTriple(target_triple);

  auto fpath = "output.o";
  std::error_code ec;
  llvm::raw_fd_ostream dest(fpath, ec, llvm::sys::fs::OF_None);

  if (ec) {
    llvm::errs() << ec.message();
    return;
  }

  llvm::legacy::PassManager pass;
  auto filetype = llvm::CodeGenFileType::ObjectFile;
  if (target_machine->addPassesToEmitFile(pass, dest, nullptr, filetype)) {
    llvm::errs() << "target machine can't emit a file of this type";
    return;
  }

  pass.run(module);
  dest.flush();
}
} // namespace Codegen
