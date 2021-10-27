ggplot(actual_qis, aes(b_timeperiodend, matssname)) + geom_pointinterval(aes(xmin = b_timeperiodend.lower, xmax = b_timeperiodend.upper, width = .width)) + facet_wrap(vars(currency), scales = "free_x") + geom_vline(xintercept = 0)


ggplot(actual_qis, aes(`b_timeperiodend:sourcecurrency`, matssname)) + geom_pointinterval(aes(xmin = `b_timeperiodend:sourcecurrency.lower`, xmax = `b_timeperiodend:sourcecurrency.upper`, width = .width)) + facet_wrap(vars(currency), scales = "free_x") + geom_vline(xintercept = 0)
