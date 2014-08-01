package de.uni_leipzig.life.icfp2014.main;

import sun.security.util.Length;
import com.google.common.base.Preconditions;
import com.google.common.collect.Lists;

import java.util.List;
import java.util.Random;

import static de.uni_leipzig.life.icfp2014.main.CellContent.POWER_PILL;
import static de.uni_leipzig.life.icfp2014.main.CellContent.GHOST;
import static java.lang.String.format;
import static de.uni_leipzig.life.icfp2014.main.CellContent.FRUIT;
import static com.google.common.base.Preconditions.checkState;
import static de.uni_leipzig.life.icfp2014.main.CellContent.LAMBDA_MAN;
import static de.uni_leipzig.life.icfp2014.main.CellContent.WALL;
import static de.uni_leipzig.life.icfp2014.main.CellContent.PILL;
import static java.lang.Math.max;
import static java.lang.Math.min;
import static com.google.common.collect.Lists.newArrayList;

public class MazeMap {
    private final int level;
    private final List<List<Integer>> map;

    public MazeMap(int level) {
        this.level = level;
        map = generateMap();
    }

    private List<List<Integer>> generateMap() {
        Random rand = new Random();
        int width = rand.nextInt(level * 100 / 2) + 1;
        int height = level * 100 / width;
        List<List<Integer>> map = simpleMap(width, height);
        insertItem(map, width, height, LAMBDA_MAN, true, "No lambda man starting position found.");
        insertItem(map, width, height, FRUIT, false, "No fruit starting position found.");
        insertItems(map, width, height, GHOST, rand.nextInt(min(level * 2, 8)) + 1);
        insertItems(map, width, height, POWER_PILL, max(2, 10 / (rand.nextInt(level) + 1)));

        return map;
    }


    private void insertItems(List<List<Integer>> map, int width, int height, CellContent itemType, int itemCount) {
        for (int i = 0; i < itemCount; i++) {
            insertItem(map, width, height, itemType,
                    true, format("No %s #%d starting position found.", itemType.toString(), i + 1));
        }
    }

    private void insertItem(List<List<Integer>> map, int width, int height, CellContent cellContent, boolean checkSafety, String errorMessage) {
        Random rand = new Random();
        int initialX = rand.nextInt(width);
        int initialY = rand.nextInt(height);
        boolean found = false;

        for (int i = 0; i < height; i++) {
            int currentY = (initialY + i) % height;

            for (int j = 0; j < width; j++) {
                int currentX = (initialX + j) % width;
                if (CellContent.valueOf(map.get(currentY).get(currentX)).isFree()
                        && (!checkSafety || areAdjacentCellsSafe(map, currentX, currentY, width, height))) {
                    map.get(currentY).set(currentX, cellContent.ordinal());
                    found = true;
                    break;
                }
            }
            if (found) {
                break;
            }
        }
        checkState(found, errorMessage);
    }

    private boolean areAdjacentCellsSafe(List<List<Integer>> map, int x, int y, int width, int height) {
        return isCellSafe(map.get(max(y - 1, 0)).get(x)) // up
                && isCellSafe(map.get(y).get(min(x + 1, width - 1))) // right
                && isCellSafe(map.get(min(y + 1, height - 1)).get(x)) // down
                && isCellSafe(map.get(y).get(max(x - 1, 0))); // left
    }

    private boolean isCellSafe(Integer ordinal) {
        return CellContent.valueOf(ordinal).isSafe();
    }

    private List<List<Integer>> simpleMap(int width, int height) {
        List<List<Integer>> map = newArrayList();

        for (int i = 0; i < height; i++) {
            List<Integer> row = simpleMapRow(width, i);
            map.add(row);
        }
        return map;
    }

    private List<Integer> simpleMapRow(int width, int rowIndex) {
        List<Integer> row = newArrayList();

        for (int j = 0; j < width; j++) {
            if ((rowIndex % 2 * j) % 2 == 0) {
                row.add(PILL.ordinal());
            } else {
                row.add(WALL.ordinal());
            }
        }
        return row;
    }

    public int getLevel() {
        return level;
    }

    public String toString() {
        StringBuilder builder = new StringBuilder(String.format("Level: %d\n", level));
        for (List<Integer> row : map) {
            for (Integer cellContent : row) {
                builder.append(CellContent.valueOf(cellContent).toSymbol());
            }
            builder.append("\n");
        }
        return builder.toString();
    }
}
