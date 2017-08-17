function [w times] = pla(x, y, ratio, method)
rs = size(x, 1);
cs = size(x, 2);
X = [ones(rs, 1) x];
w = zeros(1, cs + 1);
times = 0;
while any((Y = sign(X * w')) != y)
    if (strcmp(method, 'order'))
        r = min(find(Y != y));
    elseif (strcmp(method, 'random'))
        r = randsample(find(Y != y), 1);
    end
    w = w + ratio * y(r) * X(r, :);
    ++times;
end
