begin
    proc shiftadd(val x, y, res z) is
        z := 10 * x + y;
    end
    call shiftadd(2, 3, a);
    call shiftadd(x, 3, b);
    call shiftadd(2, y, c);
end