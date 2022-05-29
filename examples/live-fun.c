begin
    proc fst(val x, y, res z) is
        z := x;
    end
    proc snd(val x, y, res z) is
        z := y;
    end
    call fst(x, y, z);
    if z > 0 then
        skip;
    call snd(x, y, z);
    if z > 0 then
        skip;
end