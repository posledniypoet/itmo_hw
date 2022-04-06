package info.kgeorgiy.ja.kalinichev.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.*;
import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;

/**
 * Produces jar implementation of classes and interfaces
 * <br>
 * Implements {@link JarImpler}
 */
public class Implementor implements JarImpler {

    /**
     * System dependent new line symbol for writer
     */
    private static final String NEW_LINE = System.lineSeparator();

    /**
     * End of line
     */
    private static final String END_LINE = ";" + NEW_LINE;

    /**
     * Space symbol
     */
    private static final String SPACE = " ";

    /**
     * Comma symbol
     */
    private static final String COMMA = ",";

    /**
     * Decimeter for enumeration
     */
    private static final String DELIMITER = COMMA + SPACE;

    /**
     * Tab symbol
     */
    private static final String TAB = "    ";

    /**
     * Left curve bracket (opening)
     */
    private static final String LEFT_CURVE_BRACKET = "{";

    /**
     * Right curve bracket (closing)
     */
    private static final String RIGHT_CURVE_BRACKET = "}";

    /**
     * Opens new scope
     */
    private static final String OPEN_BLOCK = LEFT_CURVE_BRACKET + NEW_LINE;

    /**
     * Closes scope
     */
    private static final String CLOSE_BLOCK = RIGHT_CURVE_BRACKET + NEW_LINE;

    /**
     * Left bracket (opening)
     */
    private static final String LEFT_BRACKET = "(";

    /**
     * Right bracket (closing)
     */
    private static final String RIGHT_BRACKET = ")";

    /**
     * String with no info (for methods which should return nothing in some cases)
     */
    private static final String EMPTY_INFO = "";

    /**
     * Single indent
     */
    private static final String INDENT = TAB;

    /**
     * Double indent
     */
    private static final String DOUBLE_INDENT = TAB + TAB;

    /**
     * Open scope and make indent
     */
    private static final String OPEN_BODY = SPACE + OPEN_BLOCK + DOUBLE_INDENT;

    /**
     * Close scope and print new line
     */
    private static final String CLOSE_BODY = END_LINE + INDENT + CLOSE_BLOCK + NEW_LINE;

    /**
     * Suffix for file
     */
    private static final String FILE_SUFFIX = "Impl";

    /**
     * Java file extension
     */
    static final String JAVA_EXTENSION = ".java";

    /**
     * Class file extension
     */
    static final String CLASS_EXTENSION = ".class";

    /**
     * Appends bracket around value
     *
     * @param sb    StringBuilder buffer
     * @param value String to surround with brackets
     * @return {@link java.lang.StringBuilder} - from arguments
     */
    private StringBuilder appendWithBrackets(StringBuilder sb, String value) {
        return sb.append(LEFT_BRACKET)
                .append(value)
                .append(RIGHT_BRACKET);
    }

    /**
     * Returns Implementation Class Name
     *
     * @param token type token
     * @return {@link String} - new class name
     */
    private String getClassName(Class<?> token) {
        return token.getSimpleName() + FILE_SUFFIX;
    }

    /**
     * Converts given string to unicode escaping.
     *
     * @param data {@link String} to convert.
     * @return converted string.
     */
    static String toUnicode(String data) {
        StringBuilder builder = new StringBuilder();
        for (char c : data.toCharArray()) {
            if (c < 128) {
                builder.append(c);
            } else {
                builder.append(String.format("\\u%04X", (int) c));
            }
        }

        return builder.toString();
    }

    /**
     * Returns full path where implementation class of <code>classDefinition</code> with extension <code>extension</code> should be generated
     *
     * @param token     class type
     * @param root      root path
     * @param extension file extension
     * @return {@link Path} - file path
     * @throws IOException if error while creating directories
     */
    Path getFilePath(Class<?> token, Path root, final String extension) throws IOException {
        if (token.getPackage() != null) {
            root = root.resolve(token.getPackage().getName().replace('.', File.separatorChar) + File.separatorChar);
        }
        Files.createDirectories(root);

        return root.resolve(getClassName(token) + extension);
    }

    /**
     * Returns full path where implementation class of <code>classDefinition</code> with extension <code>extension</code> should be generated
     *
     * @param modifier     modifier of method
     * @param badModifiers modifiers which must be removed
     * @return {@link String} - modifiers string representation
     */
    private String getModifierName(int modifier, int... badModifiers) {
        for (int badModifier : badModifiers) {
            modifier &= ~badModifier;
        }

        String modifierValue = Modifier.toString(modifier);
        if (modifierValue.length() > 0) {
            return modifierValue + SPACE;
        }

        return EMPTY_INFO;
    }

    /**
     * Produces code implementing class or interface specified by provided <code>token</code>.
     * <p>
     * Generated class classes name should be same as classes name of the type token with <code>Impl</code> suffix
     * added. Generated source code should be placed in the correct subdirectory of the specified
     * <code>root</code> directory and have correct file name. For example, the implementation of the
     * interface {@link java.util.List} should go to <code>$root/java/util/ListImpl.java</code>
     *
     * @param token type token to create implementation for.
     * @param root  root directory.
     * @throws info.kgeorgiy.java.advanced.implementor.ImplerException when implementation cannot be
     *                                                                 generated.
     */
    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        if (token == null || root == null) {
            throw new ImplerException("Invalid arguments: null found");
        }
        try {
            root = getFilePath(token, root, JAVA_EXTENSION);
        } catch (IOException e) {
            throw new ImplerException("Error while creating directories for implementation file: " + e.getMessage());
        }

        try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(root.toString()), StandardCharsets.UTF_8))) {
            createImplementation(token, writer);

        } catch (UnsupportedEncodingException e) {
            throw new ImplerException("Unsupported encoding utf-8");
        } catch (FileNotFoundException e) {
            throw new ImplerException("Can't create or open file: " + root.toString() + ", " + e.getMessage());
        } catch (SecurityException e) {
            throw new ImplerException("Don't have access to write to file: " + root.toString() + ", " + e.getMessage());
        } catch (IOException e) {
            throw new ImplerException("Error while writing to: " + root.toString() + ", " + e.getMessage());
        }
    }

    /**
     * Generates full implementation of class or interface in file with suffix <code>Impl.java</code>
     * <br>
     * Prints the following: <br>
     * <ul>
     * <li>package</li>
     * <li>class declaration</li>
     * <li>constructors</li>
     * <li>methods</li>
     * </ul>
     *
     * @param token  type token
     * @param writer writer to print result
     * @throws ImplerException if error occurred during implementation
     * @throws IOException     if error occurred during implementation
     */
    private void createImplementation(Class<?> token, BufferedWriter writer) throws IOException, ImplerException {
        writePackage(token, writer);
        writeDeclaration(token, writer);
        writeConstructors(token, writer);
        writeMethods(token, writer);
    }

    /**
     * Prints package of class using
     *
     * @param token  type token
     * @param writer writer to print result
     * @throws IOException if error with IO
     */
    private void writePackage(Class<?> token, BufferedWriter writer) throws IOException {
        writer.write(toUnicode("package " + token.getPackageName() + END_LINE + NEW_LINE));
    }


    /**
     * Validates the given {@link Class} token.
     *
     * @param token the {@link Class} token of implemented class
     * @throws ImplerException if an unsupported token given
     */
    private void validateToken(Class<?> token) throws ImplerException {
        int modifiers = token.getModifiers();

        if (token.isPrimitive() || token.isArray() || token == Enum.class || Modifier.isFinal(modifiers)
                || Modifier.isPrivate(modifiers)) {
            throw new ImplerException("Given token is unsupported!");
        }
    }

    /**
     * Prints class declaration
     *
     * @param token  type token
     * @param writer writer to print result
     * @throws IOException if error with IO
     */
    private void writeDeclaration(Class<?> token, BufferedWriter writer) throws IOException, ImplerException {
        validateToken(token);
        String string = SPACE
                + "public "
                + "class"
                + SPACE
                + getClassName(token)
                + SPACE
                + (token.isInterface() ? "implements" : "extends")
                + SPACE
                + token.getCanonicalName()
                + SPACE
                + OPEN_BLOCK
                + NEW_LINE;

        writer.write(toUnicode(string));
    }

    /**
     * Prints class constructors
     *
     * @param token  type token
     * @param writer writer to print result
     * @throws IOException     if error with IO
     * @throws ImplerException if error during implementation
     */
    private void writeConstructors(Class<?> token, BufferedWriter writer) throws IOException, ImplerException {
        Constructor<?>[] constructors = token.getDeclaredConstructors();
        StringBuilder sb = new StringBuilder();

        boolean hasGoodConstructor = false;
        for (Constructor constructor : constructors) {
            if (!Modifier.isPrivate(constructor.getModifiers())) {
                sb.append(getExecutableModifiers(constructor))
                        .append(getClassName(token));

                addParametersAndExceptions(sb, constructor)
                        .append(OPEN_BODY)
                        .append("super");

                appendWithBrackets(sb, getParameters(constructor, false))
                        .append(CLOSE_BODY);

                hasGoodConstructor = true;
            }
        }

        if (!hasGoodConstructor && !token.isInterface()) {
            throw new ImplerException("Target contains only private constructors");
        }

        writer.write(toUnicode(sb.toString()));
    }

    /**
     * Converts method to {@link ComparableMethod} and removes not abstract methods
     *
     * @param methods   array of class methods to convert
     * @param collector collector to collect comparable methods in collection
     * @param <T>       collection for methods
     * @return collection of comparable methods
     */
    private <T> T methodsToComparableMethods(Method[] methods, Collector<ComparableMethod, ?, T> collector) {
        return Arrays.stream(methods)
                .filter(method -> Modifier.isAbstract(method.getModifiers()))
                .map(ComparableMethod::new)
                .collect(collector);
    }

    /**
     * Prints class methods
     *
     * @param token  type token
     * @param writer writer to print result
     * @throws IOException if error with IO
     */
    private void writeMethods(Class<?> token, BufferedWriter writer) throws IOException {
        Set<ComparableMethod> set = methodsToComparableMethods(token.getMethods(), Collectors.toCollection(HashSet::new));

        Class<?> superToken = token;
        while (superToken != null) {
            set.addAll(methodsToComparableMethods(token.getDeclaredMethods(), Collectors.toList()));
            superToken = superToken.getSuperclass();
        }

        for (ComparableMethod method : set) {
            writer.write(toUnicode(getMethod(method.getRealMethod())));
        }

        writer.write(toUnicode(CLOSE_BLOCK));
    }

    /**
     * Returns method or constructor modifiers
     *
     * @param executable method or constructor
     * @return {@link String}
     */
    private String getExecutableModifiers(Executable executable) {
        return INDENT + getModifierName(executable.getModifiers(), Modifier.ABSTRACT, Modifier.TRANSIENT, Modifier.NATIVE);
    }

    /**
     * Adds parameters and exceptions for methods and constructors
     *
     * @param sb         StringBuilder buffer
     * @param executable method or constructor
     * @return {@link StringBuilder} from arguments
     */
    private StringBuilder addParametersAndExceptions(StringBuilder sb, Executable executable) {
        appendWithBrackets(sb, getParameters(executable, true));

        Class<?>[] exceptions = executable.getExceptionTypes();
        if (exceptions.length > 0) {
            sb.append(SPACE)
                    .append("throws")
                    .append(SPACE);

            int exceptionsNumber = exceptions.length;
            for (int i = 0; i < exceptionsNumber; i++) {
                if (i > 0) {
                    sb.append(DELIMITER);
                }
                sb.append(exceptions[i].getCanonicalName());
            }
        }

        return sb;
    }

    /**
     * Returns method or constructor parameters
     *
     * @param executable method or constructor
     * @param withTypes  writer to print result
     * @return {@link String} parameters
     */
    private String getParameters(Executable executable, boolean withTypes) {
        Parameter[] parameters = executable.getParameters();

        if (parameters.length > 0) {
            StringBuilder sb = new StringBuilder();

            int paramsLength = parameters.length;
            for (int i = 0; i < paramsLength; i++) {
                if (i > 0) {
                    sb.append(DELIMITER);
                }

                if (withTypes) {
                    sb.append(parameters[i].getType().getCanonicalName()).append(SPACE);
                }
                sb.append(parameters[i].getName());
            }

            return sb.toString();
        }

        return EMPTY_INFO;
    }

    /**
     * Returns method representation as string
     *
     * @param method method
     * @return {@link String} - string representation
     */
    private String getMethod(Method method) {
        StringBuilder sb = new StringBuilder();
        for (Annotation annotation : method.getAnnotations()) {
            sb.append(annotation).append(NEW_LINE);
        }

        sb.append(getExecutableModifiers(method))
                .append(method.getReturnType().getCanonicalName())
                .append(SPACE)
                .append(method.getName());

        addParametersAndExceptions(sb, method)
                .append(OPEN_BODY)
                .append("return");

        if (!method.getReturnType().equals(void.class)) {
            sb.append(SPACE).append(getDefaultValue(method.getReturnType()));
        }

        sb.append(CLOSE_BODY);

        return sb.toString();
    }

    /**
     * Returns the default value for a given {@link Class} token.
     *
     * @param clazz the given {@link Class} token
     * @return the default value for given {@link Class} token
     */
    private static String getDefaultValue(Class<?> clazz) {
        if (!clazz.isPrimitive()) {
            return "null";
        } else if (clazz.equals(void.class)) {
            return "";
        } else if (clazz.equals(boolean.class)) {
            return "true";
        } else {
            return "0";
        }
    }

    /**
     * Helper class which provides true comparison used to store methods in {@link java.util.Set}
     */
    private class ComparableMethod {

        /**
         * Method which is wrapped
         */
        private Method method;

        /**
         * Method string view
         */
        private String stringValue;

        /**
         * Constructor from method
         *
         * @param method method to wrap
         */
        ComparableMethod(Method method) {
            this.method = method;
            this.stringValue = getMethod(method);
        }

        /**
         * Returns source method
         *
         * @return {@link Method} - source method
         */
        Method getRealMethod() {
            return method;
        }

        /**
         * Returns method hash
         *
         * @return a hash code value for this object.
         */
        @Override
        public int hashCode() {
            return stringValue.hashCode();
        }

        /**
         * Check if objects are equal
         */
        @Override
        public boolean equals(Object obj) {
            if (obj == null || getClass() != obj.getClass()) {
                return false;
            }

            return ((ComparableMethod) obj).stringValue.equals(stringValue);
        }
    }

    /**
     * Compiles class, which implements given {@code token} class.
     *
     * @param token type token to create implementation for
     * @param path  directory to store compiled file
     * @throws ImplerException if compilation fails for some reason
     */
    private void compile(final Class<?> token, final Path path) throws ImplerException {
        final String file = Path.of(path.toString(),
                token.getPackageName().replace('.',
                        File.separatorChar), token.getSimpleName() + "Impl" + ".java").toString();
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new ImplerException("Could not find java compiler");
        }

        final List<String> args = new ArrayList<>();
        args.add(file);
        args.add("-cp");
        args.add(path + File.pathSeparator + getClassPath(token));

        final int exitCode = compiler.run(null, null, null, args.toArray(String[]::new));
        if (exitCode != 0) {
            throw new ImplerException("An error occurred while compiling implementation");
        }
    }

    /**
     * Validates received arguments.
     *
     * @param args arguments given by user.
     * @return {@code true}, if all arguments are valid, else {@code false}.
     */
    static boolean validateArgs(String[] args) {
        if (args == null || args.length != 2) {
            System.err.println("Invalid arguments number, expected <class-name> <output-path>");

            return true;
        }

        for (String arg : args) {
            if (arg == null) {
                System.err.println("Invalid argument, expected not null");

                return true;
            }
        }

        return false;
    }

    /**
     * Main function which implements console interface for {@link Implementor}.
     * Pass 2 args to create source code file.
     * Pass 3 args to create jar file
     *
     * @param args console arguments
     */
    public static void main(final String[] args) {
        if (validateArgs(args)) {
            return;
        }

        if (args == null || (args.length != 2 && args.length != 3)) {
            System.err.println("2 or 3 args expected.");
            return;
        }

        if (args.length == 3 && !args[0].equals("-jar") && !args[0].equals("--jar")) {
            System.err.println(String.format("Expected -jar, found %s", args[0]));
            return;
        }

        final Implementor implementor = new Implementor();
        try {
            if (args.length == 2) {
                implementor.implement(Class.forName(args[0]), Path.of(args[1]));
            }
        } catch (final ClassNotFoundException e) {
            System.err.println("An error occurred while implementing class given: " + e.getMessage());
        } catch (final InvalidPathException e) {
            System.err.println("An error occurred while creating path for output file: " + e.getMessage());
        } catch (final ImplerException e) {
            System.err.println("An error occurred while generating implementation code for class given: " + e.getMessage());
        }
    }

    /**
     * Returns file path string representation
     *
     * @param token class to get path
     * @return file path
     */
    private static String getClassPath(Class<?> token) {
        try {
            return Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (final URISyntaxException e) {
            throw new AssertionError(e);
        }
    }

    /**
     * Creates {@code .jar} file containing compiled by {@link #compile(Class, Path)}
     * implementation of {@code token}
     *
     * @param token   class that was implemented
     * @param jarPath directory to store {@code .jar} file
     * @param tmpPath directory containing compiled implementation of {@code token}
     * @throws ImplerException if error occurs during {@link JarOutputStream} work
     */
    private void generateArtifact(final Class<?> token, final Path jarPath, final Path tmpPath) throws ImplerException {
        final Manifest manifest = new Manifest();
        manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");

        try (final JarOutputStream jos = new JarOutputStream(Files.newOutputStream(jarPath), manifest)) {
            final String name = token.getPackageName().replace('.', '/') + "/" + token.getSimpleName() + "Impl" + ".class";
            jos.putNextEntry(new ZipEntry(name));
            Files.copy(Paths.get(tmpPath.toString(), name), jos);
        } catch (final IOException e) {
            throw new ImplerException(e.getMessage());
        }
    }

    /**
     * Creates all upper directories for {@link Path} given
     *
     * @param path {@link Path} to create directories for
     * @throws IOException if directories creation fails for some reason
     */
    static void createPath(final Path path) throws IOException {
        final Path parent = path.getParent();
        if (parent != null) {
            Files.createDirectories(parent);
        }
    }

    /**
     * Implements {@code token} class and creates jar-file.
     *
     * @param token   type token to create implementation for.
     * @param jarFile target {@code .jar} file.
     * @throws ImplerException if implementation fails for some reason
     */
    @Override
    public void implementJar(final Class<?> token, final Path jarFile) throws ImplerException {
        final Path tmp;
        try {
            createPath(jarFile);
            tmp = Files.createTempDirectory(jarFile.toAbsolutePath().getParent(), "tmp");
        } catch (final IOException e) {
            throw new ImplerException(e.getMessage());
        }

        try {
            implement(token, tmp);
            compile(token, tmp);
            generateArtifact(token, jarFile, tmp);
        } finally {
            try {
                Files.walkFileTree(tmp, new Implementor.DirectoryCleanFileVisitor());
            } catch (final IOException e) {
                System.err.println("Failed to delete temporary directory");
            }
        }
    }

    /**
     * Class to recursively directory deletions. Extends {@link SimpleFileVisitor}
     */
    private static class DirectoryCleanFileVisitor extends SimpleFileVisitor<Path> {

        /**
         * Default constructor to create new instance of {@link Implementor.DirectoryCleanFileVisitor}. Uses super constructor
         */
        DirectoryCleanFileVisitor() {
            super();
        }

        /**
         * File visitor, which deletes visited files
         *
         * @param file  {@link Path} file to visit
         * @param attrs {@link BasicFileAttributes} {@code file} attributes
         * @return {@link FileVisitResult#CONTINUE}
         * @throws IOException if deletion fails for some reason
         */
        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        /**
         * Directory visitor, which deletes directory after visiting all files in it
         *
         * @param dir {@link Path} directory to visit
         * @param exc {@link IOException} instance if error occured during directory visiting
         * @return {@link FileVisitResult#CONTINUE}
         * @throws IOException if deletion fails for some reason
         */
        @Override
        public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }
    }


}
