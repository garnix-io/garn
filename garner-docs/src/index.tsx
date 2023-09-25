import {
  BrowserRouter,
  React,
  ReactDOM,
  Route,
  Routes,
} from "../deps.client.ts";
import Typescript from "./pages/typescript.tsx";
import Layout from "./components/layout.tsx";

function App() {
  return (
    <BrowserRouter>
      <Layout>
        <Routes>
          <Route path="/typescript" element={<Typescript />} />
          <Route path="/typescript/:file/:export" element={<Typescript />} />
          <Route path="*" element={<>404</>} />
        </Routes>
      </Layout>
    </BrowserRouter>
  );
}

ReactDOM.render(<App />, document.getElementById("app"));
