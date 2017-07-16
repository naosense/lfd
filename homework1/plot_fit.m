function plot_fit(w)
[x y] = meshgrid(-10:.1:10);
z = w(1) + w(2) * x + w(3) * y;
ind = find(z == 0);
hold on;
plot(x(ind), y(ind));
hold off;
endfunction
