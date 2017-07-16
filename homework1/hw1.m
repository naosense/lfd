load hw1_15_train.dat
pkg load statistics

printf("Try PLA with order\n");
[w times] = pla(hw1_15_train(:, 1:4), hw1_15_train(:, 5), 1, "order")
pause

printf("Loop PLA for 2000 times\n");
times_array = zeros(1, 2000);
times_sum = 0;
for n = 1:2000
    rand("seed", n);
    [w times] = pla(hw1_15_train(:, 1:4), hw1_15_train(:, 5), 1, "random");
    times_sum += times;
    times_array(n) = times;
end
printf("Average times is %d\n", times_sum / 2000);
pause
printf("Times hist\n");
hist(times_array);


