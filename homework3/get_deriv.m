function g = get_deriv(w, X, y, i)
N = size(X, 1);
g = zeros(size(w, 1), 1);
% for i = 1:N
% g += -1 / N * y(i) * X(i, :)' / (1 + exp(y(i) * X(i, :) * w));
% i = randi(N);
g += -1 * y(i) * X(i, :)' / (1 + exp(y(i) * X(i, :) * w));
% end
end

