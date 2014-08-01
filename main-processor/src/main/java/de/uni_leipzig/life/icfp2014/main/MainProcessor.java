package de.uni_leipzig.life.icfp2014.main;

import com.google.common.base.Preconditions;
import com.google.common.collect.Lists;
import de.uni_leipzig.life.icfp2014.ghost.GhostAI;

import java.util.List;

import static com.google.common.base.Preconditions.checkNotNull;
import static com.google.common.base.Preconditions.checkArgument;

public class MainProcessor {

    private final LambdaManAI lambdaManAI;
    private List<GhostAI> ghostAIs;
    private int level;
    
    public MainProcessor(LambdaManAI lambdaManAI, List<GhostAI> ghostAIs) {
        checkNotNull(lambdaManAI);
        checkArgument(!ghostAIs.isEmpty(), "No ghost ai given.");
        this.ghostAIs = ghostAIs;
        this.lambdaManAI = lambdaManAI;
    }
    
    public boolean runLevel(MazeMap map, int level) {
        checkNotNull(map);
        this.level = level;
        LevelState state = initializeState(map);
        while(!state.isFinished()) {
            
        }
        
        return false;
    }

    private LevelState initializeState(MazeMap map) {
        return new LevelState(map);
    }

    public class LevelState {
    
        public LevelState(MazeMap map) {
            
        }
    
        public boolean isFinished() {
            // TODO Auto-generated method stub
            return false;
        }
    
    }

}
