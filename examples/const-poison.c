begin
    proc id(val x, res y) is
        y := x;
    end
    call id(x, y);
    call id(0, z);
    if z == 0 then
        w := z + w;
    else
        w := 0;
end
