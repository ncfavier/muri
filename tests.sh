while IFS= read -r t; do
    # printf '```\n'
    printf '$ muri '\''%s'\''\n' "$t"
    ./muri "$t" 2>&1
    # printf '```\n'
    printf '\n'
done << EOF
a -> (b, c) -> (b, (a, c))
(a, b) -> Either a b
Either (a, c) (b, c) -> (Either a b, c)
(Either (a -> f) a -> f) -> f
(a -> b) -> (Either (a -> f) b -> f) -> f
(Either (a -> f) (b -> f) -> f) -> ((a, b) -> f) -> f
((a -> b) -> c) -> ((a, b -> c) -> b) -> c
a -> b
EOF
