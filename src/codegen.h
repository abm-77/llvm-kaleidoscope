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
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include <algorithm>
#include <cstdio>

namespace Codegen {

static llvm::LLVMContext ctx;
static llvm::IRBuilder<> builder(ctx);
static llvm::Module module("hex compiler", ctx);
static llvm::StringMap<llvm::Value *> named_values;

static llvm::FunctionPassManager FPM;
static llvm::LoopAnalysisManager LAM;
static llvm::FunctionAnalysisManager FAM;
static llvm::CGSCCAnalysisManager CGAM;
static llvm::ModuleAnalysisManager MAM;
static llvm::PassInstrumentationCallbacks PIC;
static llvm::StandardInstrumentations SI(ctx, true);

static void initialize_optimizations() {
  SI.registerCallbacks(PIC, &MAM);

  // simple peephole optimizations, bit twiddling
  FPM.addPass(llvm::InstCombinePass());
  // reassociate expressions
  FPM.addPass(llvm::ReassociatePass());
  // eliminate common subexpressions
  FPM.addPass(llvm::GVNPass());
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

static llvm::Value *generate(Arena *arena, AstExpr *expr) {
  auto tmp = arena->begin_temp();
  switch (expr->type) {
  case AST_NUMERIC_EXPR: {
    return llvm::ConstantFP::get(ctx, llvm::APFloat(expr->numeric_expr.val));
  } break;
  case AST_IDENTIFIER_EXPR: {
    const char *name = reinterpret_cast<const char *>(
        materialize_slice(arena, &expr->ident_expr.name));
    llvm::Value *V = named_values[name];
    if (!V)
      codegen_error("unknown variable name");
    return V;
  } break;
  case AST_INFIX_EXPR: {
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
    for (auto &arg : F->args())
      named_values[arg.getName()] = &arg;

    if (llvm::Value *ret_val = generate(arena, func.body)) {
      builder.CreateRet(ret_val);

      llvm::verifyFunction(*F);

      // FPM.run(*F, FAM);
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

    llvm::Value *startv = generate(arena, fexpr.start);
    if (!startv)
      return nullptr;

    llvm::Function *func = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *pre_header = builder.GetInsertBlock();
    llvm::BasicBlock *loopbb = llvm::BasicBlock::Create(ctx, "loop", func);
    builder.CreateBr(loopbb);

    builder.SetInsertPoint(loopbb);
    llvm::PHINode *variable =
        builder.CreatePHI(llvm::Type::getDoubleTy(ctx), 2, var_name);
    variable->addIncoming(startv, pre_header);

    llvm::Value *oldv = named_values[var_name];
    named_values[var_name] = variable;

    if (!generate(arena, fexpr.body))
      return nullptr;

    llvm::Value *stepv = (fexpr.step)
                             ? generate(arena, fexpr.step)
                             : llvm::ConstantFP::get(ctx, llvm::APFloat(1.0));
    if (!stepv)
      return nullptr;
    llvm::Value *nextv = builder.CreateFAdd(variable, stepv, "nextvar");

    llvm::Value *endcond = generate(arena, fexpr.end);
    if (!endcond)
      return nullptr;
    endcond = builder.CreateFCmpONE(
        endcond, llvm::ConstantFP::get(ctx, llvm::APFloat(0.0)), "loopcond");

    llvm::BasicBlock *loopendbb = builder.GetInsertBlock();
    llvm::BasicBlock *afterbb =
        llvm::BasicBlock::Create(ctx, "afterloop", func);
    builder.CreateCondBr(endcond, loopbb, afterbb);

    builder.SetInsertPoint(afterbb);

    variable->addIncoming(nextv, loopendbb);

    if (oldv)
      named_values[var_name] = oldv;
    else
      named_values.erase(var_name);

    return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(ctx));
  } break;
  }
  tmp.reset();
}
} // namespace Codegen
