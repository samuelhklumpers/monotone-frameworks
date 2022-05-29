begin
    proc roll(val a, b, c, d, e, res f) is
        if f < 0 then
            skip;
        f := e;    
        e := d;    
        d := c;    
        c := b;    
        b := a;
        call roll(a, b, c, d, e, f);
    end
    skip;
    call roll(a, b, c, d, e, f);
end