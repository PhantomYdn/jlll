# JLLL Swing Demos

Interactive GUI demos showcasing JLLL's Java interop capabilities.

## Running Demos

```bash
# Build the CLI JAR first
mvn package -DskipTests

# Run any demo
java -jar target/jlll-*-cli.jar demos/<demo-name>.jlll
```

## Available Demos

### hello-swing.jlll
Simple "Hello World" Swing window. Demonstrates basic Java class instantiation and method invocation.

```bash
java -jar target/jlll-*-cli.jar demos/hello-swing.jlll
```

### lambda-animation.jlll
Animated JLLL showcase with a pulsing lambda symbol. Features:
- Custom colors and fonts
- Layout management (BorderLayout, GridLayout)
- Animation loop with color transitions

```bash
java -jar target/jlll-*-cli.jar demos/lambda-animation.jlll
```

### tic-tac-toe.jlll
Interactive Tic-Tac-Toe game - play against an AI opponent! Features:
- Full game logic in JLLL
- AI with win/block/center/corner strategy  
- Atoms for mutable game state
- Button click detection via polling

```bash
java -jar target/jlll-*-cli.jar demos/tic-tac-toe.jlll
```

## Key Concepts Demonstrated

- **Java Interop**: `(class ...)`, `(new ...)`, `(invoke ...)`, `(peek-static ...)`
- **Swing Components**: JFrame, JPanel, JLabel, JButton
- **Layout Managers**: BorderLayout, GridLayout
- **Mutable State**: Atoms with `(atom ...)`, `(deref ...)`, `(reset! ...)`
- **Functional Patterns**: `map`, `filter`, `for-each`, recursion
- **Control Flow**: `when`, `cond`, `let`, `dotimes`
