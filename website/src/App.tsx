import { ServerError } from "./components/Error";
import { Main, Info } from "./pages/main";
import { Docs } from "./pages/docs";
import { BrowserRouter, Routes, Route } from "react-router-dom";

function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<Main />}>
          <Route path="docs">
            <Route index element={<Docs is_index={true} />} />
            <Route path=":docItem" element={<Docs />} />
          </Route>
          <Route index element={<Info />} />
          <Route
            path="*"
            element={
              <ServerError message="I couldn't find the page you're looking for! Please check the URL." />
            }
          />
        </Route>
      </Routes>
    </BrowserRouter>
  );
}

export default App;
