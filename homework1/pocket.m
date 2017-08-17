function [w err] = pocket(x, y, ratio, method)
rs = size(x, 1);
cs = size(x, 2);
X = [ones(rs, 1) x];
w = w_min = zeros(1, cs + 1);
err = err_min = 10;
for i = 1:100
    if (any((Y = sign(X * w')) != y))
        err = sum(Y != y) / rs;
        if (err_min > err)
            w_min = w;
            err_min = err;
        end    
        if (strcmp(method, 'order'))
            r = min(find(Y != y));
        elseif (strcmp(method, 'random'))
            r = randsample(find(Y != y), 1);
        end
        w = w + ratio * y(r) * X(r, :);
    else
        break;
    end
end
w = w_min;
err = err_min;
