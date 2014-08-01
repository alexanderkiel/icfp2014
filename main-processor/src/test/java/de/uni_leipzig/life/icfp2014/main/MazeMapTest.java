package de.uni_leipzig.life.icfp2014.main;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertThat;

public class MazeMapTest {

    @Test
    public void testGenerateMapLevel1() {
        MazeMap mazeMap = new MazeMap(1);
        assertThat(mazeMap.toString(), containsString("/"));
        assertThat(mazeMap.toString(), containsString("="));
        assertThat(mazeMap.toString(), containsString("."));
    }

    @Test
    public void testGenerateMapLevel20() {
        MazeMap mazeMap = new MazeMap(20);
        assertThat(mazeMap.toString(), containsString("/"));
        assertThat(mazeMap.toString(), containsString("="));
        assertThat(mazeMap.toString(), containsString("."));
    }

}
