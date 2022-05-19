begin
    proc mod(val x, y, res r) is
        r := x;
        while r >= y do {
            r := r - y;
        }
    end

    proc squareless(val x, res b) is
        if x <= 0 then {
            if x == 0 then
                b := 0;
            else
                b := 1;
        }
        else {
            b := 1;
            d := 0;
            while d * d < x do {
                d := d + 1;
                call mod(x, d * d, r);
                if r == 0 then {
                    b := 0;
                    break;
                }   
                else
                    skip;
            }
        }
    end
    call squareless(x, y);
end