# BBC Garbage Collector Specification

## Mechanism: Reference Counting

Every heap-allocated value has a `references: usize` field. When it reaches 0, the value is freed.

**Heap-allocated types** (tracked):
- `StringObj` — heap string with `ArrayList<u8>` content
- `BufferObj` — array of `?Value`
- `Object` — struct with `StringHashMap(Value)` habitants
- `NamespaceObj` — module with `StringHashMap(Value)` members
- `ErrorObj` — error with message string

**Stack/inline types** (never freed, no ref counting):
- `Int`, `Float`, `Bool`, `Char`, `Null`, `BuiltinFunction`
- `Function` — delegates increment/decrement to its `parentObj` if set; otherwise no-op

---

## Core API

| Call | Effect |
|------|--------|
| `value.incrementReference()` | `references += 1` |
| `value.decrementReference(allocator)` | `references -= 1`, then frees if `== 0` |
| `value.decrementReferenceNoCheck()` | `references -= 1` **without** freeing (use when value must survive) |
| `value.checkReference(allocator)` | frees if `references == 0` (used after temporary use) |
| `value.getReference()` | returns current count |

---

## Ownership Rules

### 1. Variable assignment
When a variable receives a value (`setVariable`):
- Increment the new value's reference count.
- Decrement the old value's reference count (if one existed).

### 2. Scope exit (`deinit`)
When a scope/context is destroyed:
- Decrement the reference of every variable it owns.
- This can cascade into freeing nested containers.

### 3. Temporary expression values
Expression evaluation produces a value with ref = 0 (unowned).  
Use `defer value.checkReference(allocator)` after temporary use so it is freed if no one claimed it.

### 4. Return value protection
Before destroying a child scope, protect the return value from being freed:
```
ret.incrementReference()
child_ctx.deinit()          // deinit decrements ret by 1
ret.decrementReferenceNoCheck()  // balance — restore to pre-protection count
return ret
```
This pattern appears at every scope/function boundary.

### 5. Container element ownership
- `BufferObj.setElement(idx, value)`: increment new, decrement old (if present).
- `Object.setHabitant(name, value)`: increment new, decrement old (if present).
- On container free (`deleteIfNoRef`): recursively decrement all elements/fields before freeing the container memory. This produces cascading deletion for nested structures.

### 6. Method binding
Object methods are stored **unbound** (Function with `parentObj = null`).  
When accessed via field lookup, a **bound copy** is created: `parentObj = self`.  
`incrementReference()` on a bound Function increments its parent Object.  
The caller owns this bound function value and must decrement it after the call.

Special case — when a method returns `self`:
```
if (ret.Object == parentObj) {
    function.decrementReferenceNoCheck()  // don't double-free parent
} else {
    function.decrementReference(allocator)
}
```

### 7. Method scope setup
When calling a method with a `parentObj`:
- All object habitants are injected as local variables (each increment via `setVariable`).
- `self` is set to the object (incremented).
- On scope exit, all these locals are decremented as normal.

### 8. String copy-on-write
On string concatenation (`String + Char`):
- If `lhs.references == 1` → mutate in place, return same pointer.
- Otherwise → allocate new `StringObj`, copy content, append char; ref starts at 0.

---

## Edge Cases

### Cycles
**Not handled.** Circular object references (A → B → A) will never reach ref count 0 and will leak. The compiler must either:
- Prohibit cycles at the type/analysis level, or
- Implement a secondary cycle-detection pass (e.g., mark-and-sweep over the live object graph).

### Short-circuit `And`/`Or`
If the left operand short-circuits (e.g., `true Or ...`), the left value is returned directly without a `defer checkReference`. The compiler must ensure the returned value is decremented by the caller as a normal owned return value — it must **not** also fire a defer cleanup at the evaluation site.

### `decrementReferenceNoCheck` invariant
This call lowers the ref count below the "safe" threshold without freeing. It is only valid when the caller **guarantees** the value is still reachable (e.g., it has just been incremented by the next owner). Misuse produces use-after-free.

### Function values
A `Function` with `parentObj = null` has no heap allocation — increment/decrement are no-ops.  
A `Function` with `parentObj != null` delegates all ref-count operations to that Object. The function struct itself is never heap-allocated; only the pointed-to Object matters.

### Error scopes (`try`/`catch` equivalent)
When an error is caught:
1. A new child context is created.
2. The error `Value` is assigned into it (ref incremented via `setVariable`).
3. The fallback scope runs normally.
4. Return value is protected with the standard increment/deinit/decrementNoCheck pattern.
5. The error value is decremented when the child context deinits.

---

## Freeing Logic Per Type

### `StringObj`
```
content.deinit()   // frees ArrayList<u8>
allocator.destroy(self)
```

### `BufferObj`
```
for element in content:
    element.decrementReference(allocator)
allocator.free(content)
allocator.destroy(self)
```

### `Object`
```
for (name, value) in habitants:
    value.decrementReference(allocator)
habitants.deinit()
allocator.destroy(self)
```

### `NamespaceObj`
```
for (name, value) in members:
    value.decrementReference(allocator)
members.deinit()
allocator.destroy(self)
```

### `ErrorObj`
```
allocator.free(message)
allocator.destroy(self)
```

---

## Compiler Implementation Checklist

- [ ] Every assignment emits `incrementReference(new)` + `decrementReference(old)`.
- [ ] Every scope exit emits `decrementReference` for each live variable.
- [ ] Every expression result that is not assigned uses `defer checkReference`.
- [ ] Every function/scope return uses the increment → deinit → decrementNoCheck pattern.
- [ ] Bound method values are decremented by the call site after the call.
- [ ] Method returns of `self` skip parent decrement.
- [ ] Container element writes use swap semantics (inc new, dec old).
- [ ] Container free recursively decrements all elements before freeing backing memory.
- [ ] String concatenation checks ref count == 1 before mutating in place.
- [ ] Cycle-free invariant enforced or a secondary GC pass added.
