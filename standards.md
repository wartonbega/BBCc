# Objects memory layout

When allocating an object, the memory is layed out as so:

```
+————— 8 bytes —————+———— 8 bytes —————+
| Number of ref-    | The size of the  |  // Header
| erences to the    | object.          |
| object.           |                  |
+————— 8 bytes —————+———— 8 bytes —————+
| The actual object ...       alligned | // Content
|                           at 8 bytes |
+———————————————————+——————————————————+
```

