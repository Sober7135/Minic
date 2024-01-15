#include <llvm/Support/raw_ostream.h>

#include <array>
#include <string>
#include <utility>

namespace Minic {

constexpr unsigned UINDENT = 4;
std::array<std::string, 4> BOX_DRAWING {"─", "├", "└─", "│"};

class StringWrapper {
protected:
  std::string Inner;
  unsigned Indent;

public:
  explicit StringWrapper(std::string Str, unsigned I = 0) :
      Inner(std::move(Str)),
      Indent(I) {}

  friend auto
  operator<<(llvm::raw_ostream& Out, const StringWrapper& StringWrapper)
      -> llvm::raw_ostream&& {
    auto Str = StringWrapper.Inner;
    auto Indent = StringWrapper.Indent;

    size_t start = 0;
    size_t end = Str.find('\n');

    while (end != std::string::npos) {
      Out.indent(Indent) << Str.substr(start, end - start) << '\n';
      start = end + 1;
      end = Str.find('\n', start);
    }

    Out.indent(Indent) << Str.substr(start);
    return std::move(Out);
  }

  // void printIndent(llvm::raw_ostream& Out, unsigned Indent) {
  //   auto flag = Indent % 4;
  //   while (Indent != 0) {
  //     Out << BOX_DRAWING[3] << "   ";
  //   }
  // }
};

}  // namespace Minic
