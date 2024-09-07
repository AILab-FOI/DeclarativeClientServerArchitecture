import { Component, inject, input, model, output, signal } from '@angular/core';
import { ModalData } from '../modal/modal.component';
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog';

export function openConfirmation(
  dialog: Dialog,
  data: ConfirmationData,
): DialogRef<unknown> {
  return dialog.open(ConfirmationComponent, {
    width: '',
    height: '',
    hasBackdrop: true,
    data: data,
  });
}

export type ConfirmationData = {
  title: string;
};

@Component({
  selector: 'app-confirmation',
  standalone: true,
  imports: [],
  templateUrl: './confirmation.component.html',
  styleUrl: './confirmation.component.scss',
})
export class ConfirmationComponent {
  readonly injectedData = inject<ConfirmationData>(DIALOG_DATA);
  public title = input<string>(this.injectedData.title);
  public query = output<boolean>();

  public random = this.createRandomString();

  public entered = model<string>('');

  createRandomString() {
    const chars = 'ABCDEFGHJKMNOPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';
    let result = '';
    for (let i = 0; i < 5; i++) {
      result += chars.charAt(Math.floor(Math.random() * chars.length));
    }
    return result;
  }

  close(): void {
    this.query.emit(false);
  }
  save(): void {
    this.query.emit(true);
  }
}
