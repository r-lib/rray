#!/usr/bin/env bash

cmd=$(jq -r '.tool_input.command // ""')

sinks=$(printf '%s' "$cmd" | sed -E 's#>>?[[:space:]]*/(dev|tmp)/[^[:space:];&|)]*##g')

q="\\\\?['\"]"
mode="$q[rbt]*[wax+][rwaxbt+]*$q"

script_writes=(
  "open\(.*,[[:space:]]*(mode[[:space:]]*=[[:space:]]*)?$mode"
  "\.open\([[:space:]]*$mode"
  '\.write(lines)?\('
  '\.write_(text|bytes)\('
  '\.(unlink|touch)\('
  '(os|shutil)\.(rename|replace|remove|unlink|move|copy[a-z0-9]*)\('
  'fileinput'
  '(writeFile|appendFile|createWriteStream|renameSync|unlinkSync|rmSync|copyFile)'
  'fs\.(write|rename|unlink|rm)\('
  '(File|IO)\.write'
  "File\.open\(.*,[[:space:]]*$mode"
  'FileUtils'
)
script_writes=$(IFS='|'; printf '%s' "${script_writes[*]}")

script=$(printf '%s' "$cmd" | sed -E 's#(sys\.|process\.)?std(out|err)\.write##g')

reason=''

printf '%s' "$cmd" |
  grep -Eq '(^|[[:space:];&|(])(python3?|ruby|node)[[:space:]]+(-c([[:space:]]|$)|-[[:space:]]*(<<|$))' &&
  printf '%s' "$script" | grep -Eq "$script_writes" &&
  reason='an inline interpreter script that writes files'

printf '%s' "$cmd" |
  grep -Eq '(^|[[:space:];&|(])perl[[:space:]]+[^|;&]*-[[:alnum:]]*i' &&
  reason='a perl in-place edit'

printf '%s' "$cmd" |
  grep -Eq '(^|[[:space:];&|(])sed[[:space:]]+[^|;&]*-i([[:space:]]|$)' &&
  reason='a sed in-place edit'

printf '%s' "$sinks" |
  grep -Eq '(^|[[:space:];&|(])tee([[:space:]]|$)' &&
  reason='a tee write'

printf '%s' "$sinks" |
  grep -Eq '>>?[[:space:]]*"?[^[:space:];&|"]*(\.(c|h|R|r|md|json|yml|yaml|Rd)([[:space:]]|$|;)|(src|tests|plans|man|R)/)' &&
  reason='a shell redirect into a file'

if [ -n "$reason" ]; then
  jq -n --arg reason "$reason" '{
    hookSpecificOutput: {
      hookEventName: "PreToolUse",
      permissionDecision: "deny",
      permissionDecisionReason: (
        "Blocked: this is " + $reason + ". rray4 requires the Read, Edit and Write tools for all file access and edits. Use Edit instead, with replace_all for multi-site renames. Reading with cat, sed -n and grep is still fine, as is clang-format -i and air format."
      )
    }
  }'
fi
