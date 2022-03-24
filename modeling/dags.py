import daft

# linear regression
pgm = daft.PGM()
pgm.add_node("beta", r"$\beta$", 0.5, 1.4)
pgm.add_node("xi", r"$x_{i}$", .5, .6, observed=True)
pgm.add_node("yi", r"$y_{i}$", 1.5, .6, observed=True)
pgm.add_plate([0, 0, 2, 1], label=r"$i = 1, \cdots, N$", shift=-0.1)
pgm.add_edge("xi", "yi")
pgm.add_edge("beta", "yi")
pgm.render()
pgm.savefig("lr.png", dpi=800)


# hierarchical model
pgm = daft.PGM()
pgm.add_node("beta", r"$\beta$", 0, .6)
pgm.add_node("betai", r"$\beta_{i}$", 1, .6)
pgm.add_node("yi", r"$y_{i}$", 2, .6, observed=True)
pgm.add_edge("beta", "betai")
pgm.add_edge("betai", "yi")
pgm.add_plate([.5, 0, 2, 1], label=r"$i = 1, \cdots, N$", shift=-0.1)
pgm.render()
pgm.savefig("hierarchical.png", dpi=800)

# linear regression
pgm = daft.PGM()
pgm.add_node("beta", r"$\beta$", 1, 1.4)
pgm.add_node("alpha", r"$\alpha$", 0, .6)
pgm.add_node("zi", r"$z_{i}$", 1, .6)
pgm.add_node("yi", r"$y_{i}$", 2, .6, observed=True)
pgm.add_plate([0.5, 0, 2, 1], label=r"$i = 1, \cdots, N$", shift=-0.1)
pgm.add_edge("alpha", "zi")
pgm.add_edge("zi", "yi")
pgm.add_edge("beta", "yi")
pgm.render()
pgm.savefig("latent.png", dpi=800)
