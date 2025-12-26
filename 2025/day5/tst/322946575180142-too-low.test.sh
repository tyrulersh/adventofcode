MODE=$1
# The part2 output needs to be less than this number.
MIN_RESULT=322946575180142

if [[ "$MODE" == "assert" ]]; then
  result=$(cat /dev/stdin)
  if [[ $result -le $MIN_RESULT ]]; then
    echo "Answer must be greater than $MIN_RESULT, but instead was $result"
    exit 1
  fi
fi

if [[ "$MODE" == "input" ]]; then
  cat $(dirname $0)/main-part2.input.txt
fi
