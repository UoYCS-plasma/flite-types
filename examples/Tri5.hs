{
and False x = False;
and True x = x;

filter p Nil = Nil;
filter p (Cons x xs) = case p x of {
                         True -> Cons x (filter p xs);
                         False -> filter p xs;
                       };

main = filter (and False) (Cons False Nil);
}

