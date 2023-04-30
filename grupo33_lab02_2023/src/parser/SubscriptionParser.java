package parser;

import java.io.FileReader;
import org.json.JSONObject;
import org.json.JSONTokener;

/*
 * Esta clase implementa el parser del  archivo de suscripcion (json)
 * Leer https://www.w3docs.com/snippets/java/how-to-parse-json-in-java.html
 * */

public class SubscriptionParser extends GeneralParser{

    public void parse() {
        FileReader reader = new FileReader("src/parser/subscription.json");
        JSONObject jsonObject = new JSONObject(new JSONTokener(reader));
        
        String url = jsonObject.getString("url");
        System.out.println(url);
    }
}
