begin
    x := 1;
    x := 2;
    x := x + x;
    if x > 0 then
        x := 0;
    else
        x := 0;
    while x > 0 do {
        if x < 0 then {
            continue;
            x := 2;
        }
        else
            x := 1;
    }
    skip;
end