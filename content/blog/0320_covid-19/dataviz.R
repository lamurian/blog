## LOAD PKGS

pkgs <- c('ggplot2', 'magrittr')
pkgs.load <- sapply(pkgs, library, character.only=TRUE)


## PLOT

# Case PDF
png("case-rate.png", width=800, height=800)
par(mfrow=c(2,2))
lapply(region, function(reg) {
    plot(df.group[[reg]], main=reg, ylab='Case')
    plot(df.rate[[reg]]$rate, ylab='Infection Rate')
})
par(mfrow=c(1,1))
dev.off()

# Infection rate
lapply(region, function(reg) {
    # Prepare result as a legend
    res <- fit[[reg]] %>% summary()
    msg <- paste(
        'R sq:', res$r.squared %>% round(4),
        '\np:', res$coefficients[[8]] %>% round(4)
    )
    # Create plot
    ggplot(df.rate[[reg]], aes(x=case, y=rate)) + geom_point() +
        geom_smooth(method="lm", linetype=1, colour="blue") +
        geom_smooth(method="loess", linetype=3, colour="red") +
        labs(title=reg, x="Case", y="Rate")
    ggsave(paste(reg, "rate.pdf", sep="-"), width=6, height=4)
    #text(x=4000, y=1, msg, adj=c(0,0))
})

# Healthy graph
png("h-graph.png", width=800, height=800)
plot(h.graph, vertex.size=7, vertex.color=0, layout=layout.sphere)
dev.off()

# Healthy vs sick
png("healthy-sick.png", width=1800, height=600)
par(mfrow=c(1,3))
plot(h.graph, vertex.size=7, vertex.color=0, layout=layout.sphere)
plot(h.graph, vertex.size=7, vertex.color=sim.inf$begin, layout=layout.sphere)
plot(h.graph, vertex.color=sim.contact$hazard$begin,
    vertex.size=sim.contact$hazard$begin*2 + 7,
    layout=layout.sphere
)
par(mfrow=c(1,1))
dev.off()

# Social distancing
png("soc-distance.png", width=1800, height=600)
par(mfrow=c(1,3))
plot(sd.graph, vertex.size=7, vertex.color=0, layout=layout.sphere)
plot(sd.graph, vertex.size=7, vertex.color=sim.inf$begin, layout=layout.sphere)
plot(sd.graph, vertex.color=sim.contact$soc.dist$begin,
    vertex.size=sim.contact$soc.dist$begin*2 + 7,
    layout=layout.sphere
)
par(mfrow=c(1,1))
dev.off()

# Quarantine
png("inf-prog.png", width=1080, height=1080)
par(mfrow=c(3,3))
# Hazard
plot(h.graph, vertex.color=sim.contact$hazard$begin,
    vertex.size=sim.contact$hazard$begin*2 + 7,
    layout=layout.sphere, main="Beginning phase",
    vertex.label="", ylab="Hazard population"
)
plot(h.graph, vertex.color=sim.contact$hazard$mid,
    vertex.size=sim.contact$hazard$mid*2 + 7,
    layout=layout.sphere, main="Mid phase",
    vertex.label=""
)
plot(h.graph, vertex.color=sim.contact$hazard$end,
    vertex.size=sim.contact$hazard$end*2 + 7,
    layout=layout.sphere, main="End phase",
    vertex.label=""
)
# Social distancing
plot(sd.graph, vertex.color=sim.contact$soc.dist$begin,
    vertex.size=sim.contact$soc.dist$begin*2 + 7,
    layout=layout.sphere, ylab="Social distancing",
    vertex.label=""
)
plot(sd.graph, vertex.color=sim.contact$soc.dist$mid,
    vertex.size=sim.contact$soc.dist$mid*2 + 7,
    layout=layout.sphere, vertex.label=""
)
plot(sd.graph, vertex.color=sim.contact$soc.dist$end,
    vertex.size=sim.contact$soc.dist$end*2 + 7,
    layout=layout.sphere, vertex.label=""
)
# Quarantine
plot(q.graph, vertex.color=sim.contact$quarantine$begin,
    vertex.size=sim.contact$quarantine$begin*2 + 7,
    layout=layout.sphere, ylab="Quarantine",
    vertex.label=""
)
plot(q.graph, vertex.color=sim.contact$quarantine$mid,
    vertex.size=sim.contact$quarantine$mid*2 + 7,
    layout=layout.sphere, vertex.label=""
)
plot(q.graph, vertex.color=sim.contact$quarantine$end,
    vertex.size=sim.contact$quarantine$end*2 + 7,
    layout=layout.sphere, vertex.label=""
)
par(mfrow=c(1,1))
dev.off()

# Simulated cases
png("mm-case.png", width=1600, height=800)
par(mfrow=c(1,2))
plot(mm.case$S, xlab="Iteration", ylab="Case", main="Markov Chain Simulation")
plot(rate(mm.case$S), xlab="Iteration", ylab="Infection Rate")
par(mfrow=c(1,1))
dev.off()
