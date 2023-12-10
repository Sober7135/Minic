import re

template = '''#pragma once
#include "tokenType.hh"

#include <string>

constexpr auto TokenType2String(int token) -> const std::string {{
  switch (token)  {{
{} 
  default:
    return "<Error>";
  }}
}}
'''

template_case_return =  '''  case TokenType::{}:
    return "{}";
'''

pattern = r"\s*([^,]*),\n"
case_return = ''

with open("src/tokenType.hh") as fr: 
  for line in fr:
    match = re.search(pattern, line)
    if match:
      token_type = match.group(1)
      if token_type.find(' ') == -1:
        case_return += template_case_return.format(token_type, token_type)
  
  with open("src/token2string.hh", '+w') as fw:
    fw.write(template.format(case_return)) 
