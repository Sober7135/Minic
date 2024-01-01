#include <llvm/Support/raw_ostream.h>

#include <string>
#include <utility>

namespace Minic {
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
};

}  // namespace Minic
