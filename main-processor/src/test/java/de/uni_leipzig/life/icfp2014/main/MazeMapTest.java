package de.uni_leipzig.life.icfp2014.main;

import static org.hamcrest.CoreMatchers.containsString;

import static org.junit.Assert.assertThat;

import org.hamcrest.CoreMatchers;
import org.junit.Assert;
import org.junit.Test;
import static org.junit.Assert.*;

public class MazeMapTest {

    @Test
    public void testGenerateMap() {
        MazeMap mazeMap = new MazeMap(1);
        assertThat(mazeMap.toString(), containsString("/"));
        assertThat(mazeMap.toString(), containsString("="));
        assertThat(mazeMap.toString(), containsString("."));
    }

}
