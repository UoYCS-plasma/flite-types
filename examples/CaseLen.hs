{

even n = case (==) n 0 of {
          True  -> True;
          False -> odd ((+) n 1);
          };

odd n = case (==) n 0 of {
          True  -> False;
          False -> even ((+) n 1);
          };

main = 1;

}
