package info.kgeorgiy.ja.kalinichev.walk;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;

import static java.nio.file.StandardOpenOption.*;

public class RecursiveWalk {
    private static long ERROR_HASH = 0x0000_0000_0000_0000L;
    private static long CONST_HASH = 0xff00_0000_0000_0000L;

    public static void main(String[] args) {
        //final
        //directories

        //try-with-resourses
        //ctrl+alt+l
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("Incorrect count of input arguments");
        } else {
            File outputFile = new File(args[1]);
            if (outputFile.getParentFile() != null) {
                try {
                    Files.createDirectories(outputFile.getParentFile().toPath().getFileName());
                } catch (IOException e) {
                    System.err.println("Incorrect output file");
                }
            }
            try (final BufferedWriter writer = Files.newBufferedWriter(Paths.get(args[1]), StandardCharsets.UTF_8)) {
                try (final BufferedReader reader = Files.newBufferedReader(Paths.get(args[0]), StandardCharsets.UTF_8)) {
                    try {
                        String str;
                        while ((str = reader.readLine()) != null) {
                            recursiveVisit(str, writer);
                        }
                    } catch (IOException err) {
                        System.err.println("Can't read input file");
                    }
                } catch (IOException err) {
                    System.err.println("Can't read input file");
                } catch (InvalidPathException err) {
                    System.err.println("Invalid path input file");
                } catch (Exception err) {
                    System.err.println("Some problems with first argument of command line:");
                }
            } catch (IOException err) {
                System.err.println("Can't read output file");
            } catch (InvalidPathException err) {
                System.err.println("Invalid path output file");
            } catch (Exception err) {
                System.err.println("Troubles with second argument of command line:");
            }

        }


    }

    private static long hash(final byte[] bytes, int size, long h) {
        for (int i = 0; i < size; i++) {
            h = (h << 8) + (bytes[i] & 0xff);
            final long high = h & CONST_HASH;
            if (high != 0) {
                h ^= high >> 48;
                h &= ~high;
            }
        }
        return h;

    }

    private static long hash_sum(InputStream stream) throws IOException {
        long hash_sum = ERROR_HASH;
        byte[] b = new byte[4096];
        int c;
        while ((c = stream.read(b)) >= 0) {
            hash_sum = hash(b, c, hash_sum);
        }
        return hash_sum;
    }

    public static void fileVisit(Path path, BufferedWriter writer) {
        long hash_sum = ERROR_HASH;
        try {
            InputStream reader = Files.newInputStream(path, READ);
            hash_sum = hash_sum(reader);
        } catch (IOException err) {
            System.err.println("Can't read input file");
        }
        try {
            writer.write(String.format("%016x %s%n", hash_sum, path.toString()));
        } catch (IOException e) {
            System.err.println("Can't write to output file");
        }
    }


    private static void recursiveVisit(String str, BufferedWriter writer) {
        try {
            Path begin = Path.of(str);
            try {
                Files.walkFileTree(begin, new SimpleFileVisitor<>() {
                    @Override
                    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                        fileVisit(file, writer);
                        return FileVisitResult.CONTINUE;
                    }

                    @Override
                    public FileVisitResult visitFileFailed(Path file, IOException err) {
                        try {
                            writer.write(String.format("%016x %s%n", 0, file.toString()));
                        } catch (IOException ex) {
                            System.err.println("Can't write to output file");
                        }
                        return FileVisitResult.CONTINUE;
                    }


                });
            } catch (IOException err) {
                System.err.println("WalkTree have some problems, maybe was deleted some files");
            }
        } catch (NullPointerException err) {
            try {
                writer.write(String.format("%016x %s%n", 0, str));
            } catch (IOException ex) {
                System.err.println("Can't write to output file");
            }
        } catch (InvalidPathException exc) {
            try {
                writer.write(String.format("%016x %s%n", 0, str));
            } catch (IOException ex) {
                System.err.println("Can't write to output file");
            }
        }
    }


}
