package de.uni_leipzig.life.icfp2014.main;

import static com.google.common.base.Preconditions.checkArgument;

enum CellContent {
    WALL(true, false, '#'),
    SPACE(true, true, ' '),
    PILL(true, true, '.'),
    POWER_PILL(true, false, 'o'),
    FRUIT(true, false, '%'),
    LAMBDA_MAN(true, false, '/'),
    GHOST(false, false, '=');

    private final boolean isSafe;
    private final boolean isFree;
    private char symbol;

    private CellContent(boolean isSafe, boolean isFree, char symbol) {
        this.isSafe = isSafe;
        this.isFree = isFree;
        this.symbol = symbol;

    }

    static CellContent valueOf(int ordinal) {
        checkArgument(ordinal >= 0 && ordinal < values().length, "Given ordinal %s is not in range [0,%s]",
                ordinal, values().length - 1);
        return values()[ordinal];
    }

    boolean isSafe() {
        return isSafe;
    }

    boolean isFree() {
        return isFree;
    }

    public char toSymbol() {
        return symbol;
    }
}