#!/usr/bin/env bash

cmd=$(jq -r '.tool_input.command // ""')

sinks=$(printf '%s' "$cmd" | sed -E 's#>>?[[:space:]]*/(dev|tmp)/[^[:space:];&|)]*##g')

reason=''

printf '%s' "$cmd" |
  grep -Eq '(^|[[:space:];&|(])(python3?|ruby|node)[[:space:]]+(-c([[:space:]]|$)|-[[:space:]]*(<<|$))' &&
  reason='an inline interpreter script'

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
