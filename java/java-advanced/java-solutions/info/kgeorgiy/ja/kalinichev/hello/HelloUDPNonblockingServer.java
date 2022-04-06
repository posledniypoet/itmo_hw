package info.kgeorgiy.ja.kalinichev.hello;
import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.*;
import java.nio.charset.StandardCharsets;
import java.util.Iterator;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;


public class HelloUDPNonblockingServer implements HelloServer {
    private static final int TIMEOUT_TERM = 500;

    ExecutorService manager;
    Selector selector;
    DatagramChannel channel;

    @Override
    // :NOTE: only one thread
    public void start(int port, int threads) {
        InetSocketAddress socketAddress = new InetSocketAddress(port);
        try {
            selector = Selector.open();

            channel = Utils.openDatagramChannel();
            channel.bind(socketAddress);

            channel.register(selector, SelectionKey.OP_READ, new Context());
            manager = Executors.newFixedThreadPool(threads);
            manager.submit(() -> {
                try {
                    start();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            });
        } catch (IOException e) {
            e.printStackTrace();
            System.err.println("Error");
        }
    }

    static class Context {
        ByteBuffer buffer;
        String data;
        SocketAddress clientAddress;


        void setData(String s) {
            data = s;
        }

        void updateBuffer() {
            buffer = ByteBuffer.allocate(data.length() + "Hello, ".length());
            buffer.put(("Hello, " + data).getBytes(StandardCharsets.UTF_8));
        }
    }

    private void start() throws IOException {
        // :NOTE: const buffer size
        final ByteBuffer dst = ByteBuffer.allocate(1024);
        while (channel.isOpen()) {
            selector.select(TIMEOUT_TERM);
            for (final Iterator<SelectionKey> i = selector.selectedKeys().iterator(); i.hasNext(); ) {
                final SelectionKey key = i.next();
                if (key.isReadable()) {
                    DatagramChannel channel = (DatagramChannel) key.channel();
                    Context context = (Context) key.attachment();
                    try {
                        dst.clear();
                        context.clientAddress = channel.receive(dst);
                        if (context.clientAddress != null) {
                            String msg = Utils.readStringFromBuffer(dst);
                            context.setData(msg);
                            context.updateBuffer();
                            key.interestOps(SelectionKey.OP_WRITE);
                        }
                    } catch (IOException e) {
                        System.err.println("Error in sending/receiving data" + e.getMessage());
                        key.interestOps(SelectionKey.OP_WRITE);
                    }
                }
                if (key.isValid() && key.isWritable()) {
                    DatagramChannel channel = (DatagramChannel) key.channel();
                    Context context = (Context) key.attachment();
                    context.buffer.flip();
                    channel.send(context.buffer, context.clientAddress);
                    key.interestOps(SelectionKey.OP_READ);
                }
                i.remove();
            }
        }
    }

    @Override
    public void close() {
        try {
            selector.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        try {
            channel.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        manager.shutdown();
        try {
            manager.awaitTermination(TIMEOUT_TERM, TimeUnit.MILLISECONDS);
        } catch (InterruptedException ignored) {

        }
    }

    public static void main(String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("Wrong arguments, should be 2 integer numbers");
            return;
        }

        try (HelloServer server = new HelloUDPNonblockingServer()) {
            server.start(Integer.parseInt(args[0]), Integer.parseInt(args[1]));
        } catch (NumberFormatException e) {
            System.err.println("Wrong arguments, should be 2 integer numbers");
        }
    }


}