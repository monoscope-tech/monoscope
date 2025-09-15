# Performance Optimization Plan for log-list.ts

## Critical Issues Identified

### 1. **Virtualization Configuration Issue**
- The `@lit-labs/virtualizer` v2.1.0 doesn't use `totalItems` property - this is incorrect
- The virtualizer works with the `items` array directly
- Real issue: ALL items including hidden ones are being passed to virtualizer
- In tree view, the filter `this.spanListTree.filter((e) => e.show)` runs on every render
- Sentinel values ('start', 'end') are included in the virtualized list

### 2. **Synchronous heavy computations**
- `groupSpans` and `flattenSpanTree` process all data synchronously on the main thread
- No chunking or async processing for large datasets
- Blocks UI during processing

### 3. **Inefficient Update Batching**
- Already has `batchRequestUpdate()` but it's not used consistently
- Many direct `requestUpdate()` calls bypass the batching mechanism
- The batch timer is only 16ms (one frame), too short for effective batching

### 4. **Memory leaks**
- Event listeners not properly cleaned up
- Memoization caches grow unbounded
- References to DOM elements kept in memory
- Circular references in span tree structure

### 5. **Inefficient DOM queries**
- Repeated querySelector calls in hot paths
- No caching of frequently accessed elements
- DOM refs exist but not consistently used

## High-Impact, Low-Risk Optimizations

### 1. Fix Virtualization (HIGHEST IMPACT)
**Current issue:**
```typescript
// In render():
const list: (EventLine | 'end' | 'start')[] = this.view === 'tree' 
  ? this.spanListTree.filter((e) => e.show)  // Filters on EVERY render!
  : [...this.spanListTree];
list.push('end');    // Sentinel values complicate virtualization
list.unshift('start');

${virtualize({
  items: list,  // Includes ALL items + sentinels
  renderItem: this.logItemRow,
  layout: {
    itemSize: {
      height: 28,
      width: '100%',
    },
  },
})}
```

**Correct fix needed:**
- Cache visible items to avoid filtering on every render
- Remove sentinel items from virtualized list
- Handle start/end items separately outside virtualizer
- Implement proper list updates only when data changes

**Expected impact:** 70-80% reduction in initial render time

### 2. Fix Scroll Performance Issues
**Current issues:**
- `debouncedHandleScroll` with 5ms is reasonable for smooth scrolling
- Batching updates with 50ms+ delay causes choppy, sluggish scrolling
- The real issue is what happens INSIDE the scroll handler, not the frequency

**Root causes of choppy scrolling:**
- The scroll handler calls `requestUpdate()` on every scroll event
- The entire component re-renders, including re-filtering the list
- The virtualizer recalculates everything on each render

**Correct fixes needed:**
- Remove unnecessary `requestUpdate()` calls from scroll handler
- Only update UI when scroll state actually changes (e.g., reaching top/bottom)
- Let the virtualizer handle its own scroll updates internally
- Keep scroll debounce low (5-10ms) for smooth interaction

### 3. Smart Update Batching (Use Carefully!)
**When to batch updates:**
- Multiple property changes from data fetching
- Background processes
- Non-interactive state changes

**When NOT to batch:**
- Scroll events
- User interactions (clicks, typing)
- Animations
- Any UI that needs immediate feedback

**Correct approach:**
```typescript
// Immediate updates for interactions:
handleUserClick() {
  this.someState = newValue;
  this.requestUpdate(); // Immediate!
}

// Batched updates for background changes:
handleDataFetch(data) {
  this.data1 = data.part1;
  this.data2 = data.part2;
  this.data3 = data.part3;
  this.batchRequestUpdate('data-fetch'); // Batch multiple changes
}
```

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

### Phase 1: Immediate Impact (2-4 hours)
1. **Fix virtualization**
   - Add `visibleItems` cache to avoid filtering on every render
   - Remove sentinel items from virtualized list
   - Update visible items only when data changes
   - Test with 10k+ log entries

2. **Fix scroll performance**
   - Remove `requestUpdate()` from scroll handler except when state changes
   - Keep scroll debounce LOW (5-10ms) for smooth interaction
   - Let virtualizer handle its own internal scroll updates
   - Only update UI for actual state changes (reaching top/bottom)

### Phase 2: Data Processing (4-6 hours)
3. **Optimize data processing**
   - Implement incremental updates instead of rebuilding entire tree
   - Cache DOM element references consistently
   - Optimize `expandTrace` to avoid full tree traversal

4. **Memory management**
   - Add memoization cache clearing
   - Fix event listener cleanup in `disconnectedCallback`
   - Remove circular references

### Phase 3: Polish & Testing (2-4 hours)
5. **Performance monitoring**
   - Add performance marks to measure improvements
   - Test with large datasets (10k, 50k, 100k items)
   - Profile memory usage over time

6. **Summary rendering optimization**
   - Use existing memoization more effectively
   - Reduce template complexity for badges
   - Batch DOM updates in column operations

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
- **Initial load**: 70-80% faster (by not filtering on every render)
- **Scrolling**: Smooth 60fps (by removing unnecessary requestUpdate calls)
- **Memory**: 50% reduction in usage (by fixing leaks and caching)
- **CPU**: 60% reduction during scroll (virtualizer only renders visible items)

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
// Add visible items cache
@state() private visibleItems: EventLine[] = [];
@state() private dataVersion: number = 0;  // Track data changes

// Update visible items only when data changes
private updateVisibleItems() {
  if (this.view === 'tree') {
    this.visibleItems = this.spanListTree.filter(e => e.show);
  } else {
    this.visibleItems = [...this.spanListTree];
  }
  this.dataVersion++;
}

// In render(), handle sentinels separately:
render() {
  const showRecentButton = this.recentDataToBeAdded.length > 0 && !this.flipDirection;
  const showLoadMore = this.hasMore && this.spanListTree.length > 0;
  
  return html`
    ${showRecentButton ? this.fetchRecent() : nothing}
    
    <tbody>
      ${virtualize({
        items: this.visibleItems,  // No sentinels, only actual data
        renderItem: this.logItemRow,
        layout: {
          itemSize: { height: 28, width: '100%' }
        }
      })}
    </tbody>
    
    ${showLoadMore ? this.renderLoadMore() : nothing}
  `;
}
```

### 2. Fix Scroll Performance
```typescript
// The problem: handleScroll calls requestUpdate() on EVERY scroll event!
private handleScroll(event: any) {
  const container = event.target;
  if (this.flipDirection) {
    if (container.scrollTop + container.clientHeight >= container.scrollHeight - 1) {
      this.shouldScrollToBottom = true;
    } else {
      this.shouldScrollToBottom = false;
      this.handleRecentConcatenation();
    }
  } else {
    if (container.scrollTop === 0) this.handleRecentConcatenation();
  }
  this.requestUpdate(); // THIS IS THE PROBLEM - removes smooth scrolling!
}

// FIXED VERSION:
private handleScroll(event: any) {
  const container = event.target;
  if (this.flipDirection) {
    const wasAtBottom = this.shouldScrollToBottom;
    const isAtBottom = container.scrollTop + container.clientHeight >= container.scrollHeight - 1;
    
    if (wasAtBottom !== isAtBottom) {
      this.shouldScrollToBottom = isAtBottom;
      if (!isAtBottom) this.handleRecentConcatenation();
      // Only update when state actually changes
      this.requestUpdate();
    }
  } else {
    if (container.scrollTop === 0) this.handleRecentConcatenation();
  }
  // NO requestUpdate() on every scroll!
}

// Keep debounce LOW for smooth scrolling:
this.debouncedHandleScroll = debounce(this.handleScroll.bind(this), 10); // 10ms max
```

### 3. Why Batching Causes Sluggish Scrolling
```typescript
// DON'T DO THIS - causes choppy scrolling:
private batchRequestUpdate(source: string) {
  // 50ms delay means scroll updates are delayed by 50ms!
  this.updateBatchTimer = setTimeout(() => {
    this.requestUpdate();
  }, 50); 
}

// For scroll-related updates, use immediate updates:
private handleScroll(event: any) {
  // ... scroll logic ...
  if (stateChanged) {
    this.requestUpdate(); // Immediate update for UI responsiveness
  }
}

// Batching is only good for non-interactive updates:
// - Data fetching completions
// - Background processes
// - Multiple property changes at once
// But NOT for user interactions like scrolling!
```

### 4. Memory Management Fixes
```typescript
// Clear memoization cache periodically
private clearMemoizationCache() {
  if (this.memoizedRenderSummaryElements?.cache) {
    this.memoizedRenderSummaryElements.cache.clear();
  }
}

// Call in disconnectedCallback and when data refreshes
disconnectedCallback() {
  // ... existing cleanup ...
  this.clearMemoizationCache();
  // Break circular references
  this.spanListTree = [];
  this.visibleItems = [];
  this.expandedTraces = {};
}
```

## Implementation Checklist

### Quick Wins (Do First)
- [ ] Add `visibleItems` cache state variable
- [ ] Update `render()` to remove sentinel items from virtualizer
- [ ] Fix `handleScroll` to only call `requestUpdate()` when state changes
- [ ] Keep scroll debounce at 5-10ms (NOT 50ms)
- [ ] Do NOT batch scroll-related updates

### Medium Effort
- [ ] Implement `updateVisibleItems()` method
- [ ] Call `updateVisibleItems()` only when data changes
- [ ] Optimize `handleScroll` to only update when state changes
- [ ] Add `clearMemoizationCache()` method
- [ ] Fix memory leaks in `disconnectedCallback`

### Testing & Validation
- [ ] Test with 10,000 log entries
- [ ] Verify 60fps scrolling performance
- [ ] Check memory usage doesn't grow unbounded
- [ ] Ensure all features still work (expand/collapse, live streaming, etc.)

## Next Steps

1. Start with Quick Wins - these are low risk, high impact
2. Test each change individually before moving to the next
3. Use Chrome DevTools Performance tab to measure improvements
4. Document actual performance gains vs expected
5. Share results with team before proceeding to Phase 2