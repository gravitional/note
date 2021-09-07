# object-oriented plot

from matplotlib.figure import Figure
from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas

fig    = Figure()
canvas = FigureCanvas(fig)
ax     = fig.add_axes([0.1, 0.1, 0.8, 0.8])

line,  = ax.plot([0,1], [0,1])
ax.set_title("a straight line (OO)")
ax.set_xlabel("x value")
ax.set_ylabel("y value")

canvas.print_figure('demo.jpg')