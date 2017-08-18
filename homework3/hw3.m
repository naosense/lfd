pkg load statistics

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

% nonlinear feature transform
X = [ones(N, 1) x x(:, 1) .* x(:, 2) x.^2];
w = pinv(X' * X) * X' * y;
yt = sign(X * w);
printf('Quadratic regress error is %f\n', sum(yt != y) / N);
hold on;
[x1 x2] = meshgrid(-1:.01:1);
z = w(1) + w(2) * x1 + w(3) * x2 + w(4) * (x1 .* x2) + w(5) * x1.^2 + w(6) * x2.^2;
contour(x1, x2, z, [0 0], 'r-', 'LineWidth', 2);
hold off;
