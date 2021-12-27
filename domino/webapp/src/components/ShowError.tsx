export const ShowError = (err: Error) => {
  console.error(err);

  return (
    <>
      <h1>eror!</h1>
      <p>uh oh: {err.toString()}</p>
    </>
  );
};
