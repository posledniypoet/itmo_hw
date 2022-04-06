public class Task3 {
    public static void main(String args[]){
        for(int i=1;i<=100;i++){
            int z=i*i;
            System.out.print("curl \"http://1d3p.wp.codeforces.com/new\" ^\n" +
                    "  -H \"Connection: keep-alive\" ^\n" +
                    "  -H \"Cache-Control: max-age=0\" ^\n" +
                    "  -H \"Upgrade-Insecure-Requests: 1\" ^\n" +
                    "  -H \"Origin: http://1d3p.wp.codeforces.com\" ^\n" +
                    "  -H \"Content-Type: application/x-www-form-urlencoded\" ^\n" +
                    "  -H \"User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.102 Safari/537.36\" ^\n" +
                    "  -H \"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9\" ^\n" +
                    "  -H \"Referer: http://1d3p.wp.codeforces.com/\" ^\n" +
                    "  -H \"Accept-Language: ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7\" ^\n" +
                    "  -H \"Cookie: _ga=GA1.2.355382811.1570963560; 70a7c28f3de=wkhej1gbwaaz8uf0p4; __utma=71512449.355382811.1570963560.1600109318.1600360487.106; __utmc=71512449; __utmz=71512449.1600360487.106.12.utmcsr=google^|utmccn=(organic)^|utmcmd=organic^|utmctr=(not^%^20provided); JSESSIONID=01A22F4D1E708253E8DB394290B3DD6A\" ^\n" +
                    "  --data-raw \"_af=34be50b38beccce4^&proof="+Integer.toString(z)+"^&amount="+Integer.toString(i)+"^&submit=Submit\" ^\n" +
                    "  --insecure &\n");
        }
    }
}
