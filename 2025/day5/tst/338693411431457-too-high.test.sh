MODE=$1
# The part2 output needs to be less than this number.
MAX_RESULT=338693411431457

if [[ "$MODE" == "assert" ]]; then
  result=$(cat /dev/stdin)
  if [[ $result -ge $MAX_RESULT ]]; then
    echo "Answer must be less than $MAX_RESULT, but instead was $result"
    exit 1
  fi
fi

if [[ "$MODE" == "input" ]]; then
  cat "$0.input"
fi
