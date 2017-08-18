pkg load statistics

x = 2 * rand(100,1) - 1;
noise = ones(100, 1);
selected = randsample(1:100, 20);
noise(selected, 1) = -1;
y = sign(x) .* noise;
plot(x, y, 'o');

