package hydrocul.util;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.atomic.AtomicInteger;

public class NumberObjectPool extends ObjectPool {

    private AtomicInteger counter;

    public NumberObjectPool(ScheduledExecutorService executor){
        super(executor);
        counter = new AtomicInteger(0);
    }

    @Override
    protected String createKey(){
        int c = counter.incrementAndGet();
        return Integer.toString(c);
    }

}
