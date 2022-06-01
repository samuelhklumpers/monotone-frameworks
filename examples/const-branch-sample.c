begin
    x := 0 * x;
    if x == 0 then
        x := 1;
    else
        x := 0;
    print(x);
    while x == 1 do
        x := x;
    print(x);
end