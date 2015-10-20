package com.zlatozar.cert;

import java.io.*;
import java.security.*;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.UUID;

import javax.net.ssl.*;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;

import org.apache.log4j.Logger;

public final class CertificateSslCertificateTruster {

    private static final Logger log = Logger.getLogger(CertificateSslCertificateTruster.class);

    private static final String CERT_PATH = "sys.iot-cloud.com.cer";
    private static final String X509 = "X.509";

    private static final CertificateFactory ourFactory = createFactory();

    public void trust() throws GeneralSecurityException, IOException {
        trustGiven(CERT_PATH);
    }

    public void trustGiven(@NotNull String file) throws GeneralSecurityException, IOException {

        X509Certificate x509Certificate = loadX509Certificate(file);
        appendToTruststore(new X509Certificate[]{x509Certificate});
    }

    @Null
    public X509Certificate loadX509Certificate(@NotNull String path) {
        try {
            InputStream stream = new FileInputStream(new File(path));
            try {
                return (X509Certificate) ourFactory.generateCertificate(stream);

            } finally {
                if (stream != null) {
                    stream.close();
                }
            }
        } catch (Exception e) {
            log.error("Can't add certificate for path: " + path, e);
            return null;
        }
    }

    public void doTrustToAllCertificates() throws Exception {

        log.info("Do trust all certificates!");
        Security.addProvider(new com.sun.net.ssl.internal.ssl.Provider());

        TrustManager[] trustAllCerts = new TrustManager[]{

                new X509TrustManager() {
                    public X509Certificate[] getAcceptedIssuers() {
                        return null;
                    }

                    public void checkServerTrusted(X509Certificate[] certs, String authType) throws CertificateException {
                        return;
                    }

                    public void checkClientTrusted(X509Certificate[] certs, String authType) throws CertificateException {
                        return;
                    }
                }
        };

        SSLContext sc = SSLContext.getInstance("SSL");
        sc.init(null, trustAllCerts, new SecureRandom());
        HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());

        HostnameVerifier hv = (urlHostName, session) -> {
            if (!urlHostName.equalsIgnoreCase(session.getPeerHost())) {
                log.warn("URL host '" + urlHostName + "' is different to SSLSession host '" + session.getPeerHost());
            }
            return true;
        };

        HttpsURLConnection.setDefaultHostnameVerifier(hv);
    }

    // Helper methods

    private static CertificateFactory createFactory() {
        try {
            return CertificateFactory.getInstance(X509);

        } catch (CertificateException e) {
            throw new RuntimeException("Can't initialize X.509 certificate factory", e);
        }
    }

    private void appendToTruststore(X509Certificate[] chain) throws KeyStoreException, NoSuchAlgorithmException,
            IOException, CertificateException {
        KeyStore trustStore = KeyStore.getInstance(KeyStore.getDefaultType());
        trustStore.load(null);

        int count = 0;
        for (X509Certificate cert : getDefaultTrustManager().getAcceptedIssuers()) {
            trustStore.setCertificateEntry(String.valueOf(count++), cert);
        }
        for (X509Certificate cert : chain) {
            trustStore.setCertificateEntry("" + count++, cert);
        }

        String password = UUID.randomUUID().toString();

        File trustStoreOutputFile = File.createTempFile("truststore", null);
        trustStoreOutputFile.deleteOnExit();
        trustStore.store(new FileOutputStream(trustStoreOutputFile), password.toCharArray());

        System.setProperty("javax.net.ssl.trustStore", trustStoreOutputFile.getAbsolutePath());
        System.setProperty("javax.net.ssl.trustStorePassword", password);

        log.info("Certificate was added!");
    }

    private X509TrustManager getDefaultTrustManager() throws NoSuchAlgorithmException, KeyStoreException {
        TrustManagerFactory factory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
        factory.init((KeyStore) null);

        return (X509TrustManager) factory.getTrustManagers()[0];
    }
}
