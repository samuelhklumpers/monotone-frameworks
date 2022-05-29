begin
    proc id(val x, res y) is
        y := x;
    end
    call id(x, y);
    call id(0, z);
end
