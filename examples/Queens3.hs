{
and False a = False;
and True a = a;

map f Nil = Nil;
map f (Cons x xs) = Cons (f x) (map f xs);

append Nil ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

concatMap f Nil = Nil;
concatMap f (Cons x xs) = append (f x) (concatMap f xs);

length Nil = 0;
length (Cons x xs) = (+) 1 (length xs);

toOne n = case (==) n 1 of {
            True -> Cons 1 Nil;
            False -> Cons n (toOne ((-) n 1));
          };

safe x d Nil = True;
safe x d (Cons q l) =
  and ((/=) x q) ( and ((/=) x ((+) q d)) (and ((/=) x ((-) q d)) (safe x ((+) d 1) l)));    

gen2 b q = case safe q 1 b of {
             True -> Cons (Cons q b) Nil;
             False -> Nil;
           };

gen1 nq b = concatMap (gen2 b) (toOne nq);

gen nq n =
  case (==) n 0 of {
    True -> Cons Nil Nil;  False -> concatMap (gen1 nq) (gen nq ((-) n 1));
  };
nsoln nq = length (gen nq nq);

main = emitInt (nsoln 10) 0;}





bash-4.1$ ./fl -d  examples/Queens3.hs
{
and $3 $4 = $3 [and#1,and#2] $4;
and#1 $ct $4 = False;
and#2 $ct $4 = $4;
map $5 $6 = $6 [map#1,map#2] $5;
map#1 $46 $47 $ct $5 = Cons ($5 $46) (map $5 $47);
map#2 $ct $5 = Nil;
append $9 $10 = $9 [append#1,append#2] $10;
append#1 $48 $49 $ct $10 = Cons $48 (append $49 $10);
append#2 $ct $10 = $10;
concatMap $13 $14 = $14 [concatMap#1,concatMap#2] $13;
concatMap#1 $50 $51 $ct $13 = append ($13 $50) (concatMap $13 $51);
concatMap#2 $ct $13 = Nil;
length $17 = $17 [length#1,length#2];
length#1 $52 $53 $ct = length $53 (1 (+));
length#2 $ct = 0;
toOne $20 = 1 ($20 (==)) [toOne#1,toOne#2] $20;
toOne#1 $ct $20 = Cons $20 (toOne (1 ($20 (-))));
toOne#2 $ct $20 = Cons 1 Nil;
safe $21 $22 $23 = $23 [safe#1,safe#2] $21 $22;
safe#1 $54 $55 $ct $21 $22 = and ($54 ($21 (/=))) (and ($22 ($54 (+))($21 (/=))) 
                            (and ($22 ($54 (-)) ($21 (/=))) (safe $21 (1 ($22 (+))) $55)));
safe#2 $ct $21 $22 = True;
gen2 $26 $27 = safe $27 1 $26 [gen2#1,gen2#2] $27 $26;
gen2#1 $ct $27 $26 = Nil;
gen2#2 $ct $27 $26 = Cons (Cons $27 $26) Nil;
gen1 $28 $29 = concatMap (gen2 $29) (toOne $28);
gen $30 $31 = 0 ($31 (==)) [gen#1,gen#2] $30 $31;
gen#1 $ct $30 $31 = concatMap (gen1 $30) (gen $30 (1 ($31 (-))));
gen#2 $ct $30 $31 = Cons Nil Nil;
nsoln $32 = length (gen $32 $32);
main  = nsoln 10 emitInt 0;
}

bash-4.1$ ./fl -r  examples/Queens3.hs
(0,[FUN 1 29,INT 10,PRI "emitInt",INT 0],[])
(2,[ARG 0,TAB 2 2 1,ARG 1],[])
(2,[CON 0 0],[])
(2,[ARG 1],[])
(2,[ARG 1,TAB 5 2 1,ARG 0],[])
(4,[CON 2 0,PTR 0,PTR 1],[[ARG 3,ARG 0],[FUN 2 4,ARG 3,ARG 1]])
(2,[CON 0 1],[])
(2,[ARG 0,TAB 8 2 1,ARG 1],[])
(4,[CON 2 0,ARG 0,PTR 0],[[FUN 2 7,ARG 1,ARG 3]])
(2,[ARG 1],[])
(2,[ARG 1,TAB 11 2 1,ARG 0],[])
(4,[FUN 2 7,PTR 0,PTR 1],[[ARG 3,ARG 0],[FUN 2 10,ARG 3,ARG 1]])
(2,[CON 0 1],[])
(1,[ARG 0,TAB 14 2 0],[])
(3,[FUN 1 13,ARG 1,PTR 0],[[INT 1,PRI "(+)"]])
(1,[INT 0],[])
(1,[INT 1,PTR 0,TAB 17 2 1,ARG 0],[[ARG 0,PRI "(==)"]])
(2,[CON 2 0,ARG 1,PTR 2],[[ARG 1,PRI "(-)"],[INT 1,PTR 0],[FUN 1 16,PTR 1]])
(2,[CON 2 0,INT 1,CON 0 1],[])
(3,[ARG 2,TAB 20 2 2,ARG 0,ARG 1],[])
(5,[FUN 2 1,PTR 1,PTR 12],[[ARG 3,PRI "(/=)"],[ARG 0,PTR 0],[ARG 0,PRI "(+)"],[ARG 3,PRI "(/=)"],
                           [ARG 4,PTR 2,PTR 3],[ARG 0,PRI "(-)"],[ARG 3,PRI "(/=)"],[ARG 4,PTR 5,PTR 6],
                           [ARG 4,PRI "(+)"],[INT 1,PTR 8],[FUN 3 19,ARG 3,PTR 9,ARG 1],
                           [FUN 2 1,PTR 7,PTR 10],[FUN 2 1,PTR 4,PTR 11]])
(3,[CON 0 1],[])
(2,[FUN 3 19,ARG 1,INT 1,ARG 0,TAB 23 2 2,ARG 1,ARG 0],[])
(3,[CON 0 1],[])
(3,[CON 2 0,PTR 0,CON 0 1],[[CON 2 0,ARG 1,ARG 2]])
(2,[FUN 2 10,PTR 0,PTR 1],[[FUN 2 22,ARG 1],[FUN 1 16,ARG 0]])
(2,[INT 0,PTR 0,TAB 27 2 2,ARG 0,ARG 1],[[ARG 1,PRI "(==)"]])
(3,[FUN 2 10,PTR 0,PTR 3],[[FUN 2 25,ARG 1],[ARG 2,PRI "(-)"],[INT 1,PTR 1],
                           [FUN 2 26,ARG 1,PTR 2]])
(3,[CON 2 0,CON 0 1,CON 0 1],[])
(1,[FUN 1 13,PTR 0],[[FUN 2 26,ARG 0,ARG 0]])

