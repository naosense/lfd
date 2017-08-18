function plot_fit(f, w, xlim, ylim)
[x y] = meshgrid(xlim, ylim);
z = f(w, x, y);
hold on;
contour(x, y, z, [0 0], 'LineWidth', 2);
hold off;
end
