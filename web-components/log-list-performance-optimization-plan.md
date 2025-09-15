# Performance Optimization Plan for log-list.ts

## Critical Issues Identified

### 1. **Virtualization renders ALL items**
- The virtualizer is rendering all items in the list instead of just visible ones
- Missing `totalItems` property causes it to render everything
- This is the primary cause of browser freezing with large datasets

### 2. **Synchronous heavy computations**
- `groupSpans` and `flattenSpanTree` process all data synchronously on the main thread
- No chunking or async processing for large datasets
- Blocks UI during processing

### 3. **Excessive re-renders**
- Multiple `requestUpdate()` calls without proper batching
- Every small state change triggers a full re-render
- No optimization for partial updates

### 4. **Memory leaks**
- Event listeners not properly cleaned up
- Memoization caches grow unbounded
- References to DOM elements kept in memory

### 5. **Inefficient DOM queries**
- Repeated querySelector calls in hot paths
- No caching of frequently accessed elements

## High-Impact, Low-Risk Optimizations

### 1. Fix Virtualization (HIGHEST IMPACT)
**Current issue:**
```typescript
${virtualize({
  items: list,
  renderItem: this.logItemRow,
  layout: {
    itemSize: {
      height: 28,
      width: '100%',
    },
  },
})}
```

**Fix needed:**
- Add `totalItems` property
- Implement proper scroll handling
- Configure buffer sizes for smooth scrolling

**Expected impact:** 80-90% reduction in initial render time

### 2. Batch Updates & Debouncing
**Current issues:**
- `debouncedHandleScroll` only has 5ms delay
- Multiple `requestUpdate()` calls in sequence
- No batching of state changes

**Fixes needed:**
- Increase scroll debounce to 50-100ms
- Implement update batching with requestAnimationFrame
- Consolidate state updates

### 3. Optimize Data Processing
**Current issue in `groupSpans`:**
- Processes entire dataset synchronously
- Creates many intermediate objects
- No early exit conditions

**Fixes needed:**
- Process data in chunks of 100-500 items
- Use `requestIdleCallback` for non-critical processing
- Optimize lodash chain operations

### 4. Memory Management
**Issues found:**
- `setupEventListeners()` adds listeners without proper cleanup
- Memoization never clears old entries
- Circular references in span tree

**Fixes needed:**
- Store listener references for cleanup
- Implement LRU cache for memoization
- Break circular references in `disconnectedCallback`

### 5. Optimize Summary Rendering
**Current inefficiency:**
- `renderSummaryElements` recreates templates on every call
- Complex parsing logic runs repeatedly
- Many small template literals

**Fixes needed:**
- Cache parsed summary elements
- Batch template creation
- Simplify badge rendering logic

## Implementation Order

### Phase 1: Immediate Impact (Day 1)
1. **Fix virtualization configuration**
   - Add totalItems calculation
   - Configure proper buffer zones
   - Test with large datasets

2. **Implement update batching**
   - Create centralized update queue
   - Use requestAnimationFrame
   - Reduce re-render frequency

### Phase 2: Data Processing (Day 2)
3. **Add data chunking**
   - Split `groupSpans` into async chunks
   - Show progressive loading
   - Maintain UI responsiveness

4. **Optimize hot paths**
   - Cache DOM queries
   - Reduce object allocations
   - Optimize loops

### Phase 3: Polish & Cleanup (Day 3)
5. **Memory optimization**
   - Fix event listener leaks
   - Implement proper cleanup
   - Add memory profiling

6. **Summary rendering optimization**
   - Cache template results
   - Reduce template complexity
   - Batch DOM updates

## Testing Strategy

### Performance Metrics to Track
- Initial render time
- Time to interactive
- Frame rate during scroll
- Memory usage over time
- CPU usage during updates

### Test Scenarios
1. Load 10,000 log entries
2. Rapid scrolling through entire list
3. Expand/collapse operations
4. Live streaming updates
5. Column resize operations

### Regression Testing
- Verify all existing features work
- Check event handling
- Validate data accuracy
- Test edge cases

## Expected Results

### Performance Improvements
- **Initial load**: 80-90% faster
- **Scrolling**: 60fps smooth scrolling
- **Memory**: 50% reduction in usage
- **CPU**: 70% reduction in usage

### User Experience
- No more browser freezing
- Instant response to interactions
- Smooth animations
- Better overall responsiveness

## Risk Mitigation

### Low Risk Approach
- Each optimization is independent
- Can be rolled back individually
- No changes to data structures
- Preserves all existing functionality

### Monitoring
- Add performance marks
- Log optimization metrics
- Monitor error rates
- Track user feedback

## Code Examples

### 1. Virtualization Fix
```typescript
// Add total items tracking
@state() private totalItems: number = 0;

// Update virtualize call
${virtualize({
  items: list,
  totalItems: this.totalItems,
  renderItem: this.logItemRow,
  layout: {
    itemSize: {
      height: 28,
      width: '100%',
    },
  },
  // Add buffer configuration
  scrollOptions: {
    buffer: 10, // Items to render outside viewport
    threshold: 0.5, // When to load more
  },
})}
```

### 2. Batched Updates
```typescript
private updateQueue = new Set<string>();
private updateFrame: number | null = null;

private queueUpdate(source: string) {
  this.updateQueue.add(source);
  
  if (!this.updateFrame) {
    this.updateFrame = requestAnimationFrame(() => {
      this.updateFrame = null;
      this.updateQueue.clear();
      this.requestUpdate();
    });
  }
}
```

### 3. Chunked Processing
```typescript
async function processSpansInChunks(data: any[][], chunkSize = 500) {
  const chunks = [];
  
  for (let i = 0; i < data.length; i += chunkSize) {
    chunks.push(data.slice(i, i + chunkSize));
  }
  
  const results = [];
  for (const chunk of chunks) {
    await new Promise(resolve => requestIdleCallback(resolve));
    results.push(...processChunk(chunk));
  }
  
  return results;
}
```

## Next Steps

1. Review plan with team
2. Set up performance monitoring
3. Create feature branch
4. Implement Phase 1
5. Measure improvements
6. Iterate based on results