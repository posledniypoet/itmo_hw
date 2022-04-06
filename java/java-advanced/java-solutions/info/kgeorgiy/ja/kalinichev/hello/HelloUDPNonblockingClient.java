package info.kgeorgiy.ja.kalinichev.hello;


import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.Selector;
import java.nio.channels.SelectionKey;
import java.nio.channels.DatagramChannel;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;

import static java.nio.channels.SelectionKey.OP_READ;
import static java.nio.channels.SelectionKey.OP_WRITE;

public class HelloUDPNonblockingClient implements HelloClient {
    InetSocketAddress socketAddress;
    final static int TIMEOUT_SELECTOR=100;


    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        try (Selector selector = Selector.open()) {
            socketAddress = new InetSocketAddress(host, port);

            for (int i = 0; i < threads; i++) {
                try {
                    final DatagramChannel channel = DatagramChannel.open();

                    channel.configureBlocking(false);
                    channel.register(selector, OP_WRITE, new TransferData(prefix, i, requests, channel));
                } catch (IOException e) {
                    System.err.println("ERROR: Something wrong occurred while I/O!\n" + e.getMessage());
                    return;
                }
            }

            int loaded = 0;
            while (loaded < threads) {
                try {
                    // :NOTE: move timeout to a const value
                    selector.select(TIMEOUT_SELECTOR);

                    if (selector.selectedKeys().isEmpty()) {
                        selector.keys().forEach(key -> key.interestOps(OP_WRITE));
                    }

                    for (final var i = selector.selectedKeys().iterator(); i.hasNext(); ) {
                        final SelectionKey key = i.next();
                        final TransferData data = (TransferData) key.attachment();

                        if (data.requests == data.requestId) {
                            try {
                                data.channel.close();
                            } catch (IOException ignored) {
                                // No operations
                            }

                            ++loaded;
                        } else if (data.requests > data.requestId) {
                            if (key.isReadable()) {
                                try {
                                    data.buffer.clear();
                                    data.channel.receive(data.buffer);
                                    data.buffer.flip();

                                    final String responseMessage = StandardCharsets.UTF_8.decode(data.buffer).toString();
                                    if (responseMessage.contains(data.prefix + data.threadId + "_" + data.requestId)) {
                                        data.requestId++;
                                    }
                                    // :NOTE: Exception, e.getMessage
                                } catch (Exception e) {
                                    System.err.println("ERROR:" + e.getMessage());
                                }

                                if (data.channel.isOpen()) {
                                    key.interestOps(SelectionKey.OP_WRITE);
                                }
                            } else if (key.isWritable()) {
                                try {
                                    final String requestMessage =
                                            data.prefix + data.threadId + "_" + data.requestId;

                                    data.buffer.clear();
                                    data.buffer.put(requestMessage.getBytes());
                                    data.buffer.flip();
                                    data.channel.send(data.buffer, socketAddress);
                                } catch (IOException e) {
                                    System.err.println("ERROR: Unable to send!");
                                }

                                if (data.channel.isOpen()) {
                                    key.interestOps(OP_READ);
                                }
                            }

                            i.remove();
                        }
                    }
                } catch (IOException e) {
                    System.err.println("ERROR: " + e.getMessage());

                    return;
                }
            }
        } catch (IOException e) {
            System.err.println("ERROR: Unable to close Selector!");
        }
    }





    public static void main(String[] args) {
        if (args == null || args.length != 5) {
            System.err.println("Invalid arguments");
        }

        String host = args[0];
        int port = Integer.parseInt(args[1]);
        String prefix = args[2];
        int threads = Integer.parseInt(args[3]);
        int requests = Integer.parseInt(args[4]);

        (new HelloUDPNonblockingClient()).run(host, port, prefix, threads, requests);
    }

    class TransferData {
        final ByteBuffer buffer = ByteBuffer.allocate(1024);
        int requestId;
        final String prefix;
        final int threadId;
        final int requests;
        final DatagramChannel channel;

        TransferData(final String prefix, final int threadId, final int requests, final DatagramChannel channel) {
            this.threadId = threadId;
            this.requests = requests;
            this.prefix = prefix;
            this.channel = channel;
        }
    }
}