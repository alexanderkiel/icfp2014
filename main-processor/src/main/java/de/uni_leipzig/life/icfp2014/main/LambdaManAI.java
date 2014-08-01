package de.uni_leipzig.life.icfp2014.main;

import java.util.List;


public interface LambdaManAI {
    List<Object> main (List<Object> initialWorldState, Object undocumentedThing);
    
    List<Object> step (List<Object> currentAIState, List<Object> currentWorldState);
}
