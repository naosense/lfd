load hw1_15_train.dat
load hw1_18_train.dat
load hw1_18_test.dat
pkg load statistics

printf('Run pla with order\n');
[w times] = pla(hw1_15_train(:, 1:4), hw1_15_train(:, 5), 1, 'order')
pause

printf('Run pla with random for 2000 times\n');
times_array = zeros(1, 2000);
times_sum = 0;
for n = 1:2000
    printf('.');
    rand('seed', n);
    [w times] = pla(hw1_15_train(:, 1:4), hw1_15_train(:, 5), 1, 'random');
    times_sum += times;
    times_array(n) = times;
end
printf('\n');
printf('Average times of pla is %d\n', times_sum / 2000);
pause
hist(times_array);

printf('Run pocket with order\n');
[w err] = pocket(hw1_18_train(:, 1:4), hw1_18_train(:, 5), 1, 'order')
pause

printf('Run pocket with random for 2000 times\n');
err_array = zeros(1, 2000);
err_sum = 0;
for n = 1:2000
    printf('.');
    rand('seed', n);
    [w err] = pocket(hw1_18_train(:, 1:4), hw1_18_train(:, 5), 1, 'random');
    x = hw1_18_test(:, 1:4);
    X = [ones(size(x, 1), 1) x];
    y = hw1_18_test(:, 5);
    err = sum(sign(X * w') != y) / size(X, 1);
    err_sum += err;
    err_array(n) = err;
end
printf('\n');
printf('Average error of pocket is %d\n', err_sum / 2000);
pause
hist(err_array);


