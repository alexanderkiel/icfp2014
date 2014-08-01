package de.uni_leipzig.life.icfp2014.main;

import de.uni_leipzig.life.icfp2014.ghost.DumbGhostAI;

import com.google.common.collect.Lists;
import de.uni_leipzig.life.icfp2014.ghost.GhostAI;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.google.common.collect.Lists.newArrayList;
import static java.util.Collections.singletonList;

public class Main {

    private static LambdaManAI lambdaManAI;

    public static void main(String[] args) {
        try {
            boolean won = true;
            int level = 1;
            LambdaManAI lambdaManAI = getLambdaManAI();
            List<GhostAI> ghostAIs = getGhostAIs();
            MainProcessor mainProcessor = new MainProcessor(lambdaManAI, ghostAIs);

            while(won) {
                won = mainProcessor.runLevel(getRandomMap(level), level);
                level++;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static MazeMap getRandomMap(int level) {
        return new MazeMap(level);
    }

    private static LambdaManAI getLambdaManAI() {
        return lambdaManAI;
    }

    private static List<GhostAI> getGhostAIs() {
        List<GhostAI> ghostAIs = newArrayList();

        ghostAIs.add(new DumbGhostAI());

        return ghostAIs;
    }
}
