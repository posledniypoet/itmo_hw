package info.kgeorgiy.ja.kalinichev.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;


public class HelloUDPClient implements HelloClient {
    public static void main(String[] args) {
        if (args == null || args.length != 5 || Arrays.asList(args).contains(null)) {
            System.err.println("Expected 5 non-null arguments");
        }
        try {
            String host = args[0];
            int port = Integer.parseInt(args[1]);
            String prefix = args[2];
            int numberOfThreads = Integer.parseInt(args[3]);
            int numberOfRequests = Integer.parseInt(args[4]);
            new HelloUDPClient().run(host, port, prefix, numberOfThreads, numberOfRequests);
        } catch (NumberFormatException e) {
            System.err.println("Integer argument expected:"  + e.getMessage());
        }

    }


    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        SocketAddress serverSockAddr;
        try {
            serverSockAddr = new InetSocketAddress(InetAddress.getByName(host), port);
        } catch (UnknownHostException e) {
            System.err.println("Unknown host: " + host);
            return;
        }
        ExecutorService workers = Executors.newFixedThreadPool(threads);
        for (int i = 0; i < threads; i++) {
            final int threadId = i;
            workers.submit(new Thread(() -> {
                try(DatagramSocket socket = new DatagramSocket()) {
                    socket.setSoTimeout(500);
                    byte[] dataResponse = new byte[socket.getReceiveBufferSize()];
                    DatagramPacket responsePacket = new DatagramPacket(dataResponse, socket.getReceiveBufferSize());
                    for (int requestId = 0; requestId < requests; requestId++) {
                        String requestString = prefix + threadId + "_" + requestId;
                        byte[] dataRequest = requestString.getBytes(StandardCharsets.UTF_8);
                        DatagramPacket sendPacket = new DatagramPacket(dataRequest, dataRequest.length, serverSockAddr);
                        while (true) {
                            try {
                                socket.send(sendPacket);
                                socket.receive(responsePacket);
                                String responseString = new String(responsePacket.getData(), responsePacket.getOffset(),
                                        responsePacket.getLength(), StandardCharsets.UTF_8);
                                if (responseString.contains(requestString)) {
                                    System.out.println("Response: " + responseString);
                                    break;
                                }
                            } catch (SocketTimeoutException e) {
                                System.out.println("Timeout expired: " + e.getMessage());
                            } catch (PortUnreachableException e) {
                                System.out.println("Port is unreachable due to some reason: " + e.getMessage());
                            } catch (IOException e) {
                                System.out.println("Input/output error occurred: " + e.getMessage());
                            }
                        }
                    }
                } catch (SocketException e) {
                    System.err.println("Can't open socket or socket can't be bind to the specified local port: "
                            + e.getMessage());
                }


            }







            ));
        }
        workers.shutdown();
        try {
            workers.awaitTermination(threads * requests, TimeUnit.MINUTES);
        } catch (InterruptedException ignored) {
            //ignore
        }

    }
}
