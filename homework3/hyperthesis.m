function [h p] = hyperthesis(w, X)
p = 1 ./ (1 + exp(-1 * X * w));
h = int8(p >= .5); 
h(h == 0) = -1;
end
