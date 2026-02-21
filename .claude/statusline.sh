#!/bin/bash
input=$(cat)

MODEL=$(echo "$input" | jq -r '.model.display_name // "?"')
DIR=$(echo "$input" | jq -r '.cwd // empty')
PCT=$(echo "$input" | jq -r '.context_window.used_percentage // 0' | cut -d. -f1)

# Shorten dir to basename
DIR_SHORT="${DIR##*/}"

# Git branch
BRANCH=""
if [ -n "$DIR" ] && cd "$DIR" 2>/dev/null && git rev-parse --git-dir >/dev/null 2>&1; then
    BRANCH=" $(git branch --show-current 2>/dev/null)"
fi

echo "${DIR_SHORT}${BRANCH} | ${MODEL} | ctx ${PCT}%"
