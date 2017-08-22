pkg load statistics
load hw3_train.dat
load hw3_test.dat

% 13-15
% linear regression
N = 1000;
x = 2 * rand(N, 2) - 1;
noise = rand(N, 1);
noise(noise <= .1) = -1;
noise(noise > .1) = 1;
y = sign(x(:, 1).^2 + x(:, 2).^2 - .6) .* noise;
positive = find(y >= 0);
negive = find(y < 0);
plot(x(positive, 1), x(positive, 2), 'bo', x(negive, 1), x(negive, 2), 'r+');

X = [ones(N, 1) x];
w = pinv(X' * X) * X' * y;
yt = sign(X * w);
printf('Linear regress error is %f\n', sum(yt != y) / N);
pause;
printf('Plot linear fit on data\n');
f = @(w, x, y) w(1) + w(2) * x + w(3) * y;
plot_fit(f, w, -1:.01:1, -1:.01:1);
pause;

% feature transform
X = [ones(N, 1) x x(:, 1) .* x(:, 2) x.^2];
w = pinv(X' * X) * X' * y;
yt = sign(X * w);
printf('Quadratic feature transform error is %f\n', sum(yt != y) / N);
pause;
printf('Plot quadratic fit on data\n');
f = @(w, x, y) w(1) + w(2) * x + w(3) * y + w(4) * (x .* y) + w(5) * x.^2 + w(6) * y.^2;
plot_fit(f, w, -1:.01:1, -1:.01:1);
% hold on;
% [x1 x2] = meshgrid(-1:.01:1);
% z = w(1) + w(2) * x1 + w(3) * x2 + w(4) * (x1 .* x2) + w(5) * x1.^2 + w(6) * x2.^2;
% contour(x1, x2, z, [0 0], 'r-', 'LineWidth', 2);
% hold off;

% logistic regress
alpha = .01;
T = 2000;
x = hw3_train(:, 1:end - 1);
y = hw3_train(:, end);
N = size(y, 1);
X = [ones(N, 1) x];
w = zeros(size(X, 2), 1);
err_array = zeros(T, 1);
for i = 1:T
    w = w + alpha * -1 * get_deriv(w, X, y, mod(i, N) + 1); 
    [h p] = hyperthesis(w, X);
    err_array(i) = sum(h != y) / N;
end
plot(1:T, err_array, '+');
x = hw3_test(:, 1:end - 1);
y = hw3_test(:, end);
N = size(y, 1);
X = [ones(N, 1) x];
[h p] = hyperthesis(w, X);
err = sum(h != y) / N;
printf('Logistic regression error is %f\n', err);

