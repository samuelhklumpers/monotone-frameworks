begin
    proc id(val x, res y) is
        y := x;
    end
    call id(x, y);
    call id(0, z);
    if z == 1 then
        w := z + w;
    else
        w := 0;
    print(w);
end
