#pragma once

#include "Log.hh"
#include "Visitor.hh"
#include <cstdlib>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>

namespace Minic {
static auto removeExtAndPrefix(const std::string &Str) -> std::string {
  auto i = Str.find_last_of('.');
  auto ii = Str.find_first_of('/');
  return Str.substr(ii + 1, i - ii - 1);
}

inline void genObjectFile(CodeGenVisitor &V) {
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  auto TargetTriple = llvm::sys::getDefaultTargetTriple();
  auto &TheModule = V.LW->Mod;
  TheModule->setTargetTriple(TargetTriple);

  std::string Error;
  auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);

  // Print an error and exit if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!Target) {
    panic(Error);
  }

  auto CPU = "generic";
  auto Features = "";

  llvm::TargetOptions opt;
  auto TheTargetMachine = Target->createTargetMachine(
      TargetTriple, CPU, Features, opt, llvm::Reloc::PIC_);

  TheModule->setDataLayout(TheTargetMachine->createDataLayout());

  auto FileNameWithoutExt =
      removeExtAndPrefix(V.LW->Mod->getModuleIdentifier());
  llvm::outs() << FileNameWithoutExt << "\n\n\n\n";
  auto Filename = FileNameWithoutExt + ".o";
  std::error_code EC;
  llvm::raw_fd_ostream dest(Filename, EC, llvm::sys::fs::OF_None);
  if (EC) {
    panic("Could not open file: " + EC.message());
  }

  llvm::legacy::PassManager pass;
  auto FileType = llvm::CodeGenFileType::CGFT_ObjectFile;

  if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    panic("TheTargetMachine can't emit a file of this type");
  }

  pass.run(*TheModule);
  dest.flush();

  llvm::outs() << "Wrote " << Filename << "\n";

  system(("clang " + Filename + " -o " + FileNameWithoutExt).c_str());
}
} // namespace Minic