begin
    proc both(val x, y, res z) is
        z := x + y;
    end
    call both(x, y, z);
    if z > 0 then
        skip;
    skip;
    call both(u, v, w);
end